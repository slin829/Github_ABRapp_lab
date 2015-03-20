# ABR_latency
library(shiny)
library(ggplot2)

Dir= getwd()
Dir=sub("/ABR.app", "", Dir)
#Dir= "C:/Users/clin181/Desktop/PhD/Data Analysis/2015/R"
setwd(Dir)

availf = c("clicks", "4k","8k", "12k","16k","20k","24k","28k")
avail_system= dir("./ABR_standards")
avail_folder = dir("./ABR_ID")

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  div(
    column(
      8,
      titlePanel("ABR Standard"),
      h5(HTML("Select a frequency and ID from the dropdown meun for analysis"))
    ),
    column(
      4,
      img(src="University_of_Auckland_logo.png",  width=175, heigt=76)
    )
  ),
  
  #
  # div containing 1-on the left explanation and 2-the summary
  #
  div(
    sidebarLayout(
      position = "right",
      
      # Summary get the results from server.R
      sidebarPanel( 
        h5("Summary",align="center"),
        textOutput("summary")
      ), # end  sidebarPanel
      
      # explanations on the left
      mainPanel(
        p(
          HTML("<ul>
            <li> Frequency: </li>
            <ul>
            <li> Standardised ABR wave I (baseline) latency/amplitude vs intensity for selected frequency with 95% confidence (green) and prediction interval (blue) </li>
            </ul>
            </ul>")
        ),
        p(
          HTML("<ul>
            <li> ID: </li>
            <ul>
            <li> Select the animal ID for comparision with the standardised intervals</li>
            </ul>
            </ul>")
        ),
        p(
          HTML("<ul>
            <li> System folder: </li>
            <ul>
            <li> Select the appropriate ABR system for your experiment setup</li>
            </ul>
            </ul>")
        ),
        p(
          HTML("<ul>
            <li> Data folder: </li>
            <ul>
            <li> Select the folder consisting your animal data/ID</li>
            </ul>
            </ul>")
        )
      ) # end main panel
    ) # end of sidebarLayout
  ), # end div
  
  #
  # choose the displayed frequency, data set to analyse and reference data set 
  #
  sidebarPanel(position = "left",
               selectInput("freq", label = "Frequency",
                           choices =  availf, selected = "clicks"),
               #radioButtons("ID", label = "ID",
               #choices = c("NULL", avail_id), inline=TRUE ),
               uiOutput("selectID"),
               selectInput("system", label="Select system folder:", choices=avail_system),
               selectInput("folder", label="Select data folder:", choices=avail_folder )
  ), # end sidebarPanel
  
  #
  # Show plots
  #
  mainPanel(
    tabsetPanel(
      tabPanel("ABR Latency",plotOutput("Plot", width=800,height=600)),
      tabPanel("ABR Amplitude", plotOutput("Plot_amp", width = 800, height=600))
    )
  ), # mainPanel
  
  
  div("Author: Shelly Lin, with the participation of Jerome Plumat.")
))
