# ABR_latency
library(shiny)
library(ggplot2)

Dir= getwd()
Dir=sub("/ABR.app", "", Dir)
#Dir= "C:/Users/clin181/Desktop/PhD/Data Analysis/2015/R"
setwd(Dir)

source(paste(Dir,'/latency_fit.R', sep=""))
source(paste(Dir,'/amplitude_fit.R', sep=""))
source(paste(Dir,'/createSummary.R', sep=""))

# Define server logic required 
shinyServer(function(input, output) {
  output$selectID <- renderUI({
  avail_id <- dir(paste0("./ABR_ID/", input$folder))
    radioButtons("ID", label = "ID", choices = c("NULL", avail_id), inline=TRUE ) 
  })
  
  # Plot frequency fitting
  output$Plot <- renderPlot(
    expr=latfit(input$freq, input$ID, input$folder, input$system),
    width=800,
    height=600,
    res=72
  )
  
  # Plot amplitude fitting
  output$Plot_amp <- renderPlot(
    expr=ampfit(input$freq, input$ID, input$folder, input$system),
    width=800,
    height=600,
    res=72
  )
  
  # Plot summary results
  output$summary <- function(){
    summary = createSummary(1,2);
    res = summary$res;
    
    # create the message to display
    msg = "";    
    if( res == "OK"){
      msg = paste("Data fits in the prediction with metric=", summary$metric  , ". :)");
    }else{
      msg = paste("Some data don't fit in the prediction interval, metric=" , summary$metric , ". You should have a look on: [" );
 
      for( i in 1:length(summary$freqProb) ){
        f = summary$freqProb[i];
        msg = paste0( msg, f );
        print(msg)
        if( i < length(summary$freqProb) ){
          msg = paste0(msg, ", ")
        }else{
          msg = paste0(msg,"].")
        }
      }
    }
    
    return(msg);
  } 
  
  
})
