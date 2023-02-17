# Data VFis assignment 2 --> Australia population Growth vs Covid
library("shiny")
library("shinydashboard")
library("readxl")
library("ggplot2")

# Wrangle the data

data <- read_excel("Australia Population Data.xls", 
                  "Data1", range = cell_rows(146:170), 
                  col_names = FALSE) 

headers <- read_excel("Australia Population Data.xls", 
                             "Data1", range = cell_rows(1), 
                             col_names = FALSE)

headers <- append(headers, "Quarter", 0)

colnames(data) <- headers


NSW <- data[, c(1, 2, 3, 4, 5)]
Victoria <- data[, c(1, 6, 7, 8, 9)]
Queensland <- data[, c(1, 10, 11, 12, 13)]
SouthAustralia <- data[, c(1, 14, 15, 16, 17)]
WesternAustralia <- data[, c(1, 18, 19, 20, 21)]
Tasmania <- data[, c(1, 22, 23, 24, 25)]
NorthernTerritory <- data[, c(1, 26, 27, 28, 29)]
ACT <- data[, c(1, 30, 31, 32, 33)]
Australia <- data[, c(1, 34, 35, 36)]

headers2 <- c("Quarter", "natural", "overseas", "interstate", "change")
headers3 <- c("Quarter", "natural", "overseas", "change")

colnames(NSW) <- headers2
colnames(Victoria) <- headers2
colnames(Queensland) <- headers2
colnames(SouthAustralia) <- headers2
colnames(WesternAustralia) <- headers2
colnames(Tasmania) <- headers2
colnames(NorthernTerritory) <- headers2
colnames(ACT) <- headers2
colnames(Australia) <- headers3

# Start making Shiny App

ui <- dashboardPage(
  dashboardHeader("Exploring the impacts of Covid 19 on Australia and its states 
             population growth"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidrow(
      box(width = 12, 
          title = "Population data information",
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                p("This graph represents the quarterly increase in Australias population
        from March 2017 until March 2021. Australia as a whole or each different state and 
        territory can be selected to get a state by state population growth.
        The different type of population growths can also be selected which are
        Net population increase, natuarl population increase (Births - Deaths), 
        net overseas migration and for the states and territories, interstae migration.
        "),
                
                selectInput(inputId = "State",
                            label = "Select nation or state or territory data",
                            c("NSW" = "NSW", "Victoria" = "Victoria",
                              "Queensland" = "Queensland", 
                              "South Australia" = "SouthAustralia",
                              "Western Australia" = "WesternAustralia",
                              "Tasmania" = "Tasmania", 
                              "Northern Territory" = "NorthernTerritory",
                              "ACT" = "ACT", "Australia" = "Australia")
                ),
                
                selectInput(inputId = "Type",
                            label = "Choose population growth type",
                            c("Net population Change" = "change", 
                              "Natural Change" = "natural",
                              "Overseas migration change" = "overseas",
                              "Interstae migration change" = "interstate")
                ),
                
                
                
                
              ),
              mainPanel(
                plotOutput("line") 
              )
            )
        )
         
      )
    )
  )
)
server <- function(input, output) {
  
  output$line <- renderPlot({
    if(input$State == "NSW") {
      df <- NSW
    }
    
    if(input$State == "Victoria") {
      df <- Victoria
    }
    
    if(input$State == "Queensland") {
      df <- Queensland
    }
    
    if(input$State == "SouthAustralia") {
      df <- SouthAustralia
    }
    
    if(input$State == "WesternAustralia") {
      df <- WesternAustralia
    }
    
    if(input$State == "Tasmania") {
      df <- Tasmania
    }
    
    if(input$State == "NorthernTerritory") {
      df <- NorthernTerritory
    }
    
    if(input$State == "ACT") {
      df <- ACT
    }
    
    if(input$State == "Australia") {
      df <- Australia
    }
    
    if (input$Type == "change") {
      y = df$change
    }
    
    if (input$Type == "natural") {
      y = df$natural
    }
    
    if (input$Type == "overseas") {
      y = df$overseas
    }
    
    if (input$Type == "interstate") {
      y = df$interstate
    }
    
    ggplot(data = df, aes(x = Quarter, y = y)) +
      geom_line() 
  })
  
}

shinyApp(ui = ui, server = server)



