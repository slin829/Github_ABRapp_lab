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
    radioButtons("ID", label = "ID", choices = c(avail_id), selected="NULL", inline=TRUE ) 
  })
  
  # select Freq according to availability in the ID folder
  output$selectFreq <- renderUI({
    path=paste0(Dir,"/ABR_ID/", input$folder,"/", input$ID)
    availf <- list.files(path=path ,pattern = "\\.csv$")
    availf <- sub(".csv", "", availf)
    selectInput("freq", label = "Frequency", choices =  availf, selected = "clicks")
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
