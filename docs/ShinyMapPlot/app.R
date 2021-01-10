library(utils)
library(stats)
library(methods)
library(graphics)
library(grDevices)
library(base)
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Environmental Complaints Dot Plot with Shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Choose the Year",
                  value = 2019, min = 1993, max = 2020),
      checkboxGroupInput(inputId = "type",
                         label = "Seclect the types of Complaints",
                         choiceNames = c("ABANDONED SITE","AIR POLLUTION","ASBESTOS" ,
                                         "CONSTRUCTION AND DEMOLITION", "ILLEGAL DUMPING",
                                         "NOISE COMPLAINT","PERMITS ISSUED BY DOE",
                                         "RECYCLING", "SERVICE STATIONS/STORAGE TANKS",
                                         "TOXICS HAZARDOUS MATERIALS", "VEHICLE IDLING",
                                         "WATER POLLUTION","OTHER"),
                         choiceValues = c("ABANDONED SITE","AIR POLLUTION WORK ORDER","ASBESTOS WORK ORDER" ,
                                          "CONSTRUCTION AND DEMOLITION", "ILLEGAL DUMPING WORK ORDER",
                                          "NOISE COMPLAINT","PERMITS ISSUED BY DOE WORK ORDER",
                                          "RECYCLING WORK ORDER", "SERVICE STATIONS/STORAGE TANKS WORK ORDER",
                                          "TOXICS HAZARDOUS MATERIALS WORK ORDER", "VEHICLE IDLING WORK ORDER",
                                          "WATER POLLUTION","OTHER")
                         
      ),
      actionButton(inputId = "clicks",
                   label = "Plot")
    ),
    mainPanel(
      plotOutput("MapPlot")
    )
  )
)


server <- function(input, output) {
  load(file = "CHI_map_bw.RData")
  PlotData <- read.csv("A2C_DATA_tbl_1.csv",header = T)
  PlotData_tbl <- as_tibble(PlotData)
  PlotData_tbl$COMPLAINT.DATE <- as.Date(PlotData_tbl$COMPLAINT.DATE)
  PlotData_tbl_1 <- PlotData_tbl %>% 
    mutate(YEAR = as.integer(format(COMPLAINT.DATE,"%Y")))
  
  PlotData_tbl_2 <-  eventReactive(input$clicks,{
      PlotData_tbl_1 %>% 
      filter(YEAR == input$year) %>% 
      filter(COMPLAINT.TYPE %in% input$type)
  })
  
  output$MapPlot <- renderPlot({
    CHI_map_bw + geom_point(aes(x = LONGITUDE, y = LATITUDE,  colour = COMPLAINT.TYPE), 
                            data = PlotData_tbl_2(), size = 2,alpha=0.4) +
      theme(legend.position="bottom",
            plot.title=element_text(size=20, hjust=0.5))+
      labs(title = paste("The Envrionmental Complaints in Chicago in ",input$year))
      
    })

  
}


shinyApp(ui = ui, server = server)