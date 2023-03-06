#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

#loading data
OregonFires <- read_delim("ODF_Fire_Occurrence_Data.csv") 


# Ui-how it should look like
ui <- fluidPage(
  
  #title on top
  titlePanel("Oregon Wildfires"),
  
  
  #Define tabs
  #tabs for each section
  
  #Opening tab
  tabsetPanel(
    tabPanel("General Information", 
             h1("Problem Set 6"),
             h2(em(strong("Project Overview"))),
             p("This report will provide a broad summary of wildfires in the state of Oregon and its causes."),
             
             p(strong("This app uses wild fire data from DATA.GOV. The data set from the Oregon Department of Forestry (ODF) and it spans from 2000 through 2022")),
             a("Access link here", href='https://catalog.data.gov/dataset/odf-fire-occurrence-data-2000-2022'),
             p(strong("Here are a few sample lines from the data set ")),
             img(text = "Wildfire in Oregon", src = "https://www.gannett-cdn.com/presto/2020/09/09/PRRS/a48ee05d-31ba-4841-bb7b-f37200cfede4-AP20252712646829.jpg?width=1320&height=882&fit=crop&format=pjpg&auto=webp", 
                 height="50%", width="50%"),
             verbatimTextOutput("summary")
             ),
    
  #plot tab
  tabPanel("Plot", 
             mainPanel( # main panal
               p(strong("You can analyze the estimated acres lost for different districts of oregon. Select the regions you are interested in. You see a monthly scatterplot and the corresponding trend lines.")),
               plotOutput("Plot"),
                        textOutput("Response"),
             ),
           
             
             # Define sidebar panel
             sidebarPanel(
               selectInput(
                 "districts", "Select district",
                 choices = unique(OregonFires$DistrictName),
                 selected = unique(OregonFires$DistrictName)[1]
                 
               ),
               sliderInput("range", 
                           "What range of lost acres to plot: ", 
                           min = 0, 
                           max = 50000, 
                           value = c(0, 50000),
                           step = 10
               ),
             ),
           
  
  ),
    
    tabPanel("Table",
             mainPanel(tableOutput("Table")
                       ), #main panel
             
             #sidebar panel
             sidebarPanel(
               radioButtons(
                 "causes", "Fires caused by:",
                 choices = unique(OregonFires$CauseBy),
                 selected = unique(OregonFires$CauseBy) [1]
                            ),
               textOutput("RowCount"),
                          )
            )
             
)
)

#create a server
server <- function(input, output){
  
  
  #General info
  output$summary <- renderPrint({
    OregonFires %>%
    sample_n(10)
  })
  
  
  
  #plot
  #filter
  filtered_data <- reactive({
    OregonFires %>% 
      filter(!is.na(EstTotalAcres)) %>%  # filter out missing values for acres
      filter(DistrictName %in% input$districts)
  })
  
  
  # scatter plot
  output$Plot <- renderPlot({
    
    Districts <- input$districts
    
    ggplot(filtered_data(), aes(x = FireYear, y = EstTotalAcres, col = Districts)) +
      geom_point(size=2)+
      coord_cartesian(ylim = input$range) +
      labs(
           x = "Years",
           y = "Estimated Total Acres") 
  })
   # y axis range message
  output$Response <- renderText({
    if (input$range[1] >= 0 & input$range[2] < 25000) {
    paste( "\n\nNote: Y-axis range is set to less than 25000 acres of land.", sep = "")
    } else {
      paste( "\n\nNote: Y-axis range is set to more than 25000 acres of land.", sep = "")
    }
  }) 
  
  # for table
  filtered_data2 <- reactive({
    OregonFires %>%
      filter(CauseBy == input$causes) %>%
      select(FireYear, DistrictName, FireName, Size_class) 
  })
  
  output$Table <- renderTable({
    filtered_data2()
  })
output$RowCount <- renderText({
  paste("\n\nNumber of rows:", nrow(filtered_data2()))
  
})
  
}
  


  
 
  

# Run the application 
shinyApp(ui = ui, server = server)
