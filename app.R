#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list=ls())
library(googlesheets)
library(ggplot2)
gs_auth()
key <- gs_url("https://docs.google.com/spreadsheets/d/1-gmnqMlwJK_Yf7x1A2dCGItaG9NI4lFOrkwuUzmEQnA/edit#gid=544374812")
epc <- gs_read(key, ws = "Data Mapping",range = "A1:G124",col_names =  TRUE)
colnames(epc)<- c("issue","issue1","issue2","issue3","cp1","cp2","ds")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Epicenter"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        uiOutput("issue"),
        uiOutput("issue1"),
        uiOutput("issue2"),
        uiOutput("issue3")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tags$p("Check-point 1:"),
        verbatimTextOutput("cp1"),
        tags$p("Check-point 2:"),
        verbatimTextOutput("cp2"),
        tags$p("Data to be shared when mailing Epicenter for this Issue:"),
        verbatimTextOutput("ds")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Dropdowns for SH, GM, ESM and Property
    output$issue <- renderUI({
      selectInput(
        inputId = "i",
        label = "Select issue type:",
        choices = c("No value",levels(factor(epc$issue)))
      )
    })
    output$issue1 <- renderUI({
      selectInput(
        inputId = "i1",
        label = "Select sub issue type-1:",
        choices = c("No value",levels(factor(epc$issue1[epc$issue==input$i])))
      )
    })
    output$issue2 <- renderUI({
      selectInput(
        inputId = "i2",
        label = "Select sub issue type-2:",
        choices = c("No value",levels(factor(epc$issue2[(epc$issue==input$i) & (epc$issue1==input$i1)])))
      )
    })
    
    output$issue3 <- renderUI({
      selectInput(
        inputId = "i3",
        label = "Select sub issue type-3:",
        choices = c("No value",levels(factor(epc$issue3[(epc$issue==input$i) & (epc$issue1==input$i1)
                                             & (epc$issue2==input$i2)])))
      )
    })
    
    #main panel
    
    output$cp1 <- renderText({
      epc$cp1[(epc$issue==input$i) & (epc$issue1==input$i1)
              & (epc$issue2==input$i2)& (epc$issue3==input$i3)]
    })
  
    output$cp2 <- renderText({
      epc$cp2[(epc$issue==input$i) & (epc$issue1==input$i1)
              & (epc$issue2==input$i2)& (epc$issue3==input$i3)]
    })
    
    output$ds <- renderText({
      epc$ds[(epc$issue==input$i) & (epc$issue1==input$i1)
              & (epc$issue2==input$i2)& (epc$issue3==input$i3)]
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

