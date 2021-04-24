library(shiny)
library(tidycensus)
library(tidyverse)
library(tigris)
library(mapview)
library(dplyr)
library(leaflet)
options(tigris_use_cache = TRUE)
key <- "7360ddbb0f07f32de07b273190ba9e543e8a206d" #needed for census API data

#manipulating the dataset
energyUse2010 <- read.csv("energy-usage-2010.csv", header = TRUE, sep = ",")
energyUse2010$CENSUS.BLOCK <- as.character(energyUse2010$CENSUS.BLOCK)
energyUse2010[is.na(energyUse2010)] <- 0
energyUse2010 <- energyUse2010 %>% rename(GEOID = CENSUS.BLOCK)


commAreas <- c(unique(energyUse2010$COMMUNITY.AREA.NAME))

westSide <- subset(energyUse2010, energyUse2010$COMMUNITY.AREA.NAME == "Near West Side")

#census data to join based on GEOID
decen <- get_decennial(geography = "block", state = "IL", variables = "P001001",
                       county = "Cook", geometry = TRUE)

ui <- navbarPage("CS 424 Project Three",
    navbarMenu("Near West Side",
      tabPanel("Heat Map",
        fluidRow(
          column(2,
            fluidRow(
              selectInput("property", "Select which property
                          to view", c("Electricity",
                                             "Gas",
                                             "Building Age",
                                             "Building Height",
                                             "Total Population"), selected = "Electricity"),
              selectInput("month", "Select a month to view",
                          c(month.name, "Total"), selected = "Total"),
              selectInput("type", "Select a building type to view",
                          c("Residential", "Commerical", "Industrial", "All"),
                          selected = "All")
          )
        ),
          column(10,
            fluidRow(
              mapviewOutput("wsMap")
          )
        )
      )
    )
  ),
  tabPanel("About",
           h2("Author of code"),
           p("The code for this Shiny App was written by Andres Tapia. At the time of this release,
              I am currently in my second semester of my third year at the University of Illinois
              at Chicago.")
  )
)

server <- function(input, output) {
  
  wsProperty <- reactive({
    if(input$property == "Electricity")
    {
      if(input$month == "January")
        wsOption <- "KWH.JANUARY.2010"
      else if (input$month == "February")
        wsOption <- "KWH.FEBRUARY.2010"
      else if(input$month == "March")
        wsOption <- "KWH.MARCH.2010"
      else if (input$month == "April")
        wsOption <- "KWH.APRIL.2010"
      else if (input$month == "May")
        wsOption <- "KWH.MAY.2010"
      else if(input$month == "June")
        wsOption <- "KWH.JUNE.2010"
      else if (input$month == "July")
        wsOption <- "KWH.JULY.2010"
      else if (input$month == "August")
        wsOption <- "KWH.AUGUST.2010"
      else if(input$month == "September")
        wsOption <- "KWH.SEPTEMBER.2010"
      else if (input$month == "October")
        wsOption <- "KWH.OCTOBER.2010"
      else if (input$month == "November")
        wsOption <- "KWH.NOVEMBER.2010"
      else if (input$month == "December")
        wsOption <- "KWH.DECEMBER.2010"
      else if (input$month == "Total")
        wsOption <- "TOTAL.KWH" 
    }
    else if (input$property == "Gas")
    {
      ## April was spelled wrong in the dataset
      if(input$month == "January")
        wsOption <- "THERM.JANUARY.2010"
      else if (input$month == "February")
        wsOption <- "THERM.FEBRUARY.2010"
      else if(input$month == "March")
        wsOption <- "THERM.MARCH.2010"
      else if (input$month == "April")
        wsOption <- "TERM.APRIL.2010"
      else if (input$month == "May")
        wsOption <- "THERM.MAY.2010"
      else if(input$month == "June")
        wsOption <- "THERM.JUNE.2010"
      else if (input$month == "July")
        wsOption <- "THERM.JULY.2010"
      else if (input$month == "August")
        wsOption <- "THERM.AUGUST.2010"
      else if(input$month == "September")
        wsOption <- "THERM.SEPTEMBER.2010"
      else if (input$month == "October")
        wsOption <- "THERM.OCTOBER.2010"
      else if (input$month == "November")
        wsOption <- "THERM.NOVEMBER.2010"
      else if (input$month == "December")
        wsOption <- "THERM.DECEMBER.2010"
      else if (input$month == "Total")
        wsOption <- "TOTAL.THERMS" 
    }
    else if (input$property == "Building Age")
    {
      wsOption <- "AVERAGE.BUILDING.AGE"
    }
    else if (input$property == "Building Height")
    {
      wsOption <- "AVERAGE.STORIES"
    }
    else if (input$property == "Total Population")
    {
      wsOption <- "TOTAL.POPULATION"
    }
    return(wsOption)
  })
  
  westSideReactive <- reactive({
    if(input$type == "All")
    {
      westSidev2 <- westSide %>% select(GEOID, COMMUNITY.AREA.NAME, wsProperty())
      westSidev2 <- decen %>% inner_join(westSidev2, "GEOID")
    }
    else if (input$type == "Residential")
    {
      wsSubset <- subset(westSide, westSide$BUILDING.TYPE == "Residential")
      westSidev2 <- wsSubset %>% select(GEOID, COMMUNITY.AREA.NAME, 
                                        BUILDING.TYPE, wsProperty())
      westSidev2 <- decen %>% inner_join(westSidev2, "GEOID")
    }
    else if (input$type == "Commercial")
    {
      
    }
  })
  
  ### rendering the west side map
  output$wsMap <- renderLeaflet({
    wsData <- westSideReactive()
    mWS <- mapview(wsData, zcol = wsProperty())
     mWS@map
  })
  
}

shinyApp(ui = ui, server = server)