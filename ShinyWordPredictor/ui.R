#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            actionButton("word1", label = textOutput("value1")),
            actionButton("word2", label = textOutput("value2")),
            actionButton("word3", label = textOutput("value3"))
    ),
    
    # Text entry box
    mainPanel(
            textInput(inputId = "text", label = h3("Text input"), value = "Enter text..."),
            hr()
            )
    )
  )
)
