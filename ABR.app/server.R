# ABR_app
library(shiny)
library(ggplot2)

Dir= getwd()

source(paste(Dir,'/funs/latency_fit.R', sep=""))
source(paste(Dir,'/funs/amplitude_fit.R', sep=""))
source(paste(Dir,'/funs/createSummary.R', sep=""))

Dir=sub("/ABR.app", "", Dir)
#Dir= "C:/Users/clin181/Desktop/PhD/Data Analysis/2015/R"
setwd(Dir)



# Define server logic required 
shinyServer(function(input, output) {
  output$selectID <- renderUI({
  avail_id <- dir(paste0("./ABR_ID/", input$folder))
    radioButtons("ID", label = "ID", choices = c("NULL", avail_id), selected="NULL", inline=TRUE ) 
  })
  
  # select Freq according to species
  output$selectFreq<- renderUI({
    if(input$species == "Mice/Rats"){
      availf <- c("clicks", "4k", "8k", "12k", "16k", "20k", "24k", "28k")
      radioButtons("freq", label = "Frequency", choices =  availf, selected = "clicks",inline=TRUE)
    }else if(input$species == "Guinea Pigs"){
      availf <- c("clicks", "1k", "2k", "4k", "8k",  "16k", "32k")
      radioButtons("freq", label = "Frequency", choices =  availf, selected = "clicks",inline=TRUE)
    }
    
  })

  
  # Plot latency fitting
  output$Plot <- renderPlot(
    expr=latfit(input$freq, input$ID, input$folder, input$system, input$ymin, input$ymax, input$points),
    width=800,
    height=600,
    res=72
  )
  
  # Plot amplitude fitting
  output$Plot_amp <- renderPlot(
    expr=ampfit(input$freq, input$ID, input$folder, input$system, input$ymin, input$ymax,input$points),
    width=800,
    height=600,
    res=72
  )
  
  # Plot summary results
  output$summary <- function(){
    summary = createSummary(refDataID=input$system, newDataFolder=input$folder, newDataID=input$ID);
    res = summary$res;
    
    # create the message to display
    msg = "";    
    if( res == "OK"){
      msg = paste("Data fits in the prediction. :)");
    
    
    }else if( res == "KO"){
        
      msg = paste("Some data don't fit in the prediction intervals. You should have a look on:" );
 
      for( i in 1:length(summary$freqProb) ){
        f = summary$freqProb[i];
        id = which( f %in% summary$foundFreq );
        print(paste(f, summary$foundFreq))
        #print("-------")
        #print(id); 
        #print(summary$metric$nAmp[id])
        msg = paste0( msg, f, " #Points outside prediction:(amp: ", summary$metric$nAmp[i], " lat:", summary$metric$nLat[i], ")");
        #print(msg)
        if( i < length(summary$freqProb) ){
          msg = paste0(msg, "\n")
        }else{
          msg = paste0(msg)
        }
      }
    
    }else{
      msg = "No data selected."
    }# end if else TODO: change to switch case
    
    return(msg);
  } 
  
  
})
