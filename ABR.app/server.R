# ABR_latency
library(shiny)
library(ggplot2)

Dir= getwd()
Dir=sub("/ABR.app", "", Dir)
#Dir= "C:/Users/clin181/Desktop/PhD/Data Analysis/2015/R"
setwd(Dir)

source(paste(Dir,'/latency_fit.R', sep=""))
source(paste(Dir,'/amplitude_fit.R', sep=""))


# Define server logic required 
shinyServer(function(input, output) {
  output$selectID <- renderUI({
  avail_id <- dir(paste0("./ABR_ID/", input$folder))
    radioButtons("ID", label = "ID", choices = c("NULL", avail_id), inline=TRUE ) 
  })
  
  output$Plot <- renderPlot(
    expr=latfit(input$freq, input$ID, input$folder, input$system),
    width=800,
    height=600,
    res=72
  )
  output$Plot_amp <- renderPlot(
    expr=ampfit(input$freq, input$ID, input$folder, input$system),
    width=800,
    height=600,
    res=72
  )
  
  
  
})
