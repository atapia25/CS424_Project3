library(shiny)
library(tidycensus)
library(tidyverse)
library(tigris)
library(mapview)
library(dplyr)
library(leaflet)
library(ggplot2)
library(DT)
library(RColorBrewer)
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

tract <- tracts("IL", county = "Cook", year = 2010)
tract <- tract %>% rename(GEOID = GEOID10)

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
                          c("Residential", "Commercial", "Industrial", "All"),
                          selected = "All")
          )
        ),
          column(10,
            fluidRow(
              h2("Note: the button at the bottom right is a reset button"),
              mapviewOutput("wsMap")
          )
        )
      )
    ),
    tabPanel("Graph and Table",
      fluidRow(
        column(1,
          fluidRow(
            selectInput("propGT", "Select which property to view",
                        c("Electricity", "Gas"), selected = "Electricity")
          )
        ),
        column(6,
          fluidRow(
            plotOutput("wsPlot")
          )
        ),
        column(5,
          fluidRow(
            dataTableOutput("wsTable")
          )
        )
      )
    )
  ),
  navbarMenu("Community Area comparison",
    tabPanel("Heat Map",
      fluidRow(
        column(1,
          fluidRow(
            selectInput("areaLeft", "Select which community area to view",
                        commAreas, selected = "Near West Side"),
            selectInput("propLeft", "Select which property
                          to view", c("Electricity",
                                      "Gas",
                                      "Building Age",
                                      "Building Height",
                                      "Total Population"), selected = "Electricity"),
            selectInput("monthLeft", "Select a month to view",
                        c(month.name, "Total"), selected = "Total"),
            selectInput("typeLeft", "Select a building type to view",
                        c("Residential", "Commercial", "Industrial", "All"),
                        selected = "All")
            )
          ),
        column(5,
          fluidRow(
            mapviewOutput("leftMap")
            )
          ),
        column(5,
          fluidRow(
            mapviewOutput("rightMap")
            )
          ),
        column(1,
           fluidRow(
             selectInput("areaRight", "Select which community area to view",
                         commAreas, selected = "Loop"),
             selectInput("propRight", "Select which property
                      to view", c("Electricity",
                                  "Gas",
                                  "Building Age",
                                  "Building Height",
                                  "Total Population"), selected = "Electricity"),
             selectInput("monthRight", "Select a month to view",
                         c(month.name, "Total"), selected = "Total"),
             selectInput("typeRight", "Select a building type to view",
                         c("Residential", "Commercial", "Industrial", "All"),
                         selected = "All"))
        )
      )
    ),
    tabPanel("Graphs and Tables",
      fluidRow(
        column(1,
           fluidRow(
             selectInput("propGTLeft", "Select which property to view",
                         c("Electricity", "Gas"), selected = "Electricity")
          )
        ),
        column(5,
          fluidRow(
            plotOutput("leftPlot"),
            dataTableOutput("leftTable")
          )
        ),
        column(5,
          fluidRow(
            plotOutput("rightPlot"),
            dataTableOutput("rightTable")
          )
        ),
        column(1,
          fluidRow(
            selectInput("propGTRight", "Select which property to view",
                        c("Electricity", "Gas"), selected = "Electricity")
          )
        )
      )
    )
  ),
  tabPanel("City of Chicago",
    fluidRow(
      column(1,
        fluidRow(
          selectInput("levelLeft", "Select a level to view",
                      c("Block", "Tract"), selected = "Block"),
          selectInput("areaTotalLeft", "Select an area to view",
                      c(commAreas, "All"), selected = "All"),
          selectInput("propertyTotalLeft", "Select which property to view",
                      c("Electricity",
                        "Gas",
                        "Building Age",
                        "Building Height",
                        "Total Population"), selected = "Electricity")
        )
      ),
      column(5,
        fluidRow(
          mapviewOutput("totalMapLeft")
        )
      )
    )
  ),
  tabPanel("About",
           h1("Information about data"),
           p("The data consists of electrical power usage in the city of Chicago
             in 2010. The power comes from general electricty usage as well as coming
             from thermal energy. The data also contains the types of builings,
             neighborhoods in Chicago such as the Loop, Lincoln Park, and many
             others. This is vital because it could shed some light on how exactly
             electricity is used within the city of Chicago."),
           br(),
           p("The data can be downloaded for your own use at this ",
             a("link from Kaggle,",
               href = "https://www.kaggle.com/chicago/chicago-energy-usage-2010"),
             "and it can also be downloaded from the City of Chicago website ",
             a("right here.",
               href = "https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp")),
           br(),
           h2("Author of code"),
           p("The code for this Shiny App was written by Andres Tapia. At the time of this release,
              I am currently in my second semester of my third year at the University of Illinois
              at Chicago.")
  )
)

server <- function(input, output) {
  
  propertySelect <- reactive({
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
  
  ### ------- the west side data reactive functions -------- ### 
  westSideReactive <- reactive({
    if(input$type == "All")
    {
      westSidev2 <- westSide %>% select(GEOID, COMMUNITY.AREA.NAME, 
                                        propertySelect())
      westSidev2 <- decen %>% inner_join(westSidev2, "GEOID")
    }
    else if (input$type == "Residential")
    {
      wsSubset <- subset(westSide, westSide$BUILDING.TYPE == "Residential")
      westSidev2 <- wsSubset %>% select(GEOID, COMMUNITY.AREA.NAME, 
                                        BUILDING.TYPE, propertySelect())
      westSidev2 <- decen %>% inner_join(westSidev2, "GEOID")
    }
    else if (input$type == "Commercial")
    {
      wsSubset <- subset(westSide, westSide$BUILDING.TYPE == "Commercial")
      westSidev2 <- wsSubset %>% select(GEOID, COMMUNITY.AREA.NAME, 
                                        BUILDING.TYPE, 
                                        propertySelect())
      westSidev2 <- decen %>% inner_join(westSidev2, "GEOID")
    }
    else if (input$type == "Industrial")
    {
      wsSubset <- subset(westSide, westSide$BUILDING.TYPE == "Industrial")
      westSidev2 <- wsSubset %>% select(GEOID, COMMUNITY.AREA.NAME, 
                                        BUILDING.TYPE, 
                                        propertySelect())
      westSidev2 <- decen %>% inner_join(westSidev2, "GEOID")
    }
  })
  
  westSideReactive2 <- reactive({
    if (input$propGT == "Electricity")
    {
      return(westSide[5:16])
    }
    else if (input$propGT == "Gas")
    {
      return(westSide[20:31])
    }
  })
  
  ### -------- For selecting the property of the left side of the comparison ----- ###
  propSelectLeft <- reactive({
    if(input$propLeft == "Electricity")
    {
      if(input$monthLeft == "January")
        leftOption <- "KWH.JANUARY.2010"
      else if (input$monthLeft == "February")
        leftOption <- "KWH.FEBRUARY.2010"
      else if(input$monthLeft == "March")
        leftOption <- "KWH.MARCH.2010"
      else if (input$monthLeft == "April")
        leftOption <- "KWH.APRIL.2010"
      else if (input$monthLeft == "May")
        leftOption <- "KWH.MAY.2010"
      else if(input$monthLeft == "June")
        leftOption <- "KWH.JUNE.2010"
      else if (input$monthLeft == "July")
        leftOption <- "KWH.JULY.2010"
      else if (input$monthLeft == "August")
        leftOption <- "KWH.AUGUST.2010"
      else if(input$monthLeft == "September")
        leftOption <- "KWH.SEPTEMBER.2010"
      else if (input$monthLeft == "October")
        leftOption <- "KWH.OCTOBER.2010"
      else if (input$monthLeft == "November")
        leftOption <- "KWH.NOVEMBER.2010"
      else if (input$monthLeft == "December")
        leftOption <- "KWH.DECEMBER.2010"
      else if (input$monthLeft == "Total")
        leftOption <- "TOTAL.KWH" 
    }
    else if (input$propLeft == "Gas")
    {
      ## April was spelled wrong in the dataset
      if(input$monthLeft == "January")
        leftOption <- "THERM.JANUARY.2010"
      else if (input$monthLeft == "February")
        leftOption <- "THERM.FEBRUARY.2010"
      else if(input$monthLeft == "March")
        leftOption <- "THERM.MARCH.2010"
      else if (input$monthLeft == "April")
        leftOption <- "TERM.APRIL.2010"
      else if (input$monthLeft == "May")
        leftOption <- "THERM.MAY.2010"
      else if(input$monthLeft == "June")
        leftOption <- "THERM.JUNE.2010"
      else if (input$monthLeft == "July")
        leftOption <- "THERM.JULY.2010"
      else if (input$monthLeft == "August")
        leftOption <- "THERM.AUGUST.2010"
      else if(input$monthLeft == "September")
        leftOption <- "THERM.SEPTEMBER.2010"
      else if (input$monthLeft == "October")
        leftOption <- "THERM.OCTOBER.2010"
      else if (input$monthLeft == "November")
        leftOption <- "THERM.NOVEMBER.2010"
      else if (input$monthLeft == "December")
        leftOption <- "THERM.DECEMBER.2010"
      else if (input$monthLeft == "Total")
        leftOption <- "TOTAL.THERMS" 
    }
    else if (input$propLeft == "Building Age")
    {
      leftOption <- "AVERAGE.BUILDING.AGE"
    }
    else if (input$propLeft == "Building Height")
    {
      leftOption <- "AVERAGE.STORIES"
    }
    else if (input$propLeft == "Total Population")
    {
      leftOption <- "TOTAL.POPULATION"
    }
    return(leftOption)
  })
  
  ### ------- the left side reactive functions ------- ###
  leftSideReactive <- reactive({
    if(input$typeLeft == "All")
    {
      leftSide <- subset(energyUse2010, 
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaLeft)
      leftSidev2 <- leftSide %>% select(GEOID, COMMUNITY.AREA.NAME,
                                        propSelectLeft())
      leftSidev2 <- decen %>% inner_join(leftSidev2, "GEOID")
    }
    else if (input$typeLeft == "Residential")
    {
      leftSide <- subset(energyUse2010, 
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaLeft
                         & energyUse2010$BUILDING.TYPE == "Residential")
      leftSidev2 <- leftSide %>% select(GEOID, COMMUNITY.AREA.NAME,
                                        BUILDING.TYPE, propSelectLeft())
      leftSidev2 <- decen %>% inner_join(leftSidev2, "GEOID")
    }
    else if (input$typeLeft == "Commercial")
    {
      leftSide <- subset(energyUse2010, 
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaLeft
                         & energyUse2010$BUILDING.TYPE == "Commercial")
      leftSidev2 <- leftSide %>% select(GEOID, COMMUNITY.AREA.NAME,
                                        BUILDING.TYPE, propSelectLeft())
      leftSidev2 <- decen %>% inner_join(leftSidev2, "GEOID")
    }
    else if (input$typeLeft == "Industrial")
    {
      leftSide <- subset(energyUse2010, 
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaLeft
                         & energyUse2010$BUILDING.TYPE == "Industrial")
      leftSidev2 <- leftSide %>% select(GEOID, COMMUNITY.AREA.NAME,
                                        BUILDING.TYPE, propSelectLeft())
      leftSidev2 <- decen %>% inner_join(leftSidev2, "GEOID")
    }
  })
  
  leftSideReactive2 <- reactive({
    if (input$propGTLeft == "Electricity")
    {
      leftSide <- subset(energyUse2010,
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaLeft)
      return(leftSide[5:16])
    }
    else if (input$propGTLeft == "Gas")
    {
      leftSide <- subset(energyUse2010,
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaLeft)
      return(leftSide[20:31])
    }
  })
  
  ### --------- For selecting the property of the right side of the comparison --------- ###
  propSelectRight <- reactive({
    if(input$propRight == "Electricity")
    {
      if(input$monthRight == "January")
        rightOption <- "KWH.JANUARY.2010"
      else if (input$monthRight == "February")
        rightOption <- "KWH.FEBRUARY.2010"
      else if(input$monthRight == "March")
        rightOption <- "KWH.MARCH.2010"
      else if (input$monthRight == "April")
        rightOption <- "KWH.APRIL.2010"
      else if (input$monthRight == "May")
        rightOption <- "KWH.MAY.2010"
      else if(input$monthRight == "June")
        rightOption <- "KWH.JUNE.2010"
      else if (input$monthRight == "July")
        rightOption <- "KWH.JULY.2010"
      else if (input$monthRight == "August")
        rightOption <- "KWH.AUGUST.2010"
      else if(input$monthRight == "September")
        rightOption <- "KWH.SEPTEMBER.2010"
      else if (input$monthRight == "October")
        rightOption <- "KWH.OCTOBER.2010"
      else if (input$monthRight == "November")
        rightOption <- "KWH.NOVEMBER.2010"
      else if (input$monthRight == "December")
        rightOption <- "KWH.DECEMBER.2010"
      else if (input$monthRight == "Total")
        rightOption <- "TOTAL.KWH" 
    }
    else if (input$propRight == "Gas")
    {
      ## April was spelled wrong in the dataset
      if(input$monthRight == "January")
        rightOption <- "THERM.JANUARY.2010"
      else if (input$monthRight == "February")
        rightOption <- "THERM.FEBRUARY.2010"
      else if (input$monthRight == "March")
        rightOption <- "THERM.MARCH.2010"
      else if (input$monthRight == "April")
        rightOption <- "TERM.APRIL.2010"
      else if (input$monthRight == "May")
        rightOption <- "THERM.MAY.2010"
      else if (input$monthRight == "June")
        rightOption <- "THERM.JUNE.2010"
      else if (input$monthRight == "July")
        rightOption <- "THERM.JULY.2010"
      else if (input$monthRight == "August")
        rightOption <- "THERM.AUGUST.2010"
      else if (input$monthRight == "September")
        rightOption <- "THERM.SEPTEMBER.2010"
      else if (input$monthRight == "October")
        rightOption <- "THERM.OCTOBER.2010"
      else if (input$monthRight == "November")
        rightOption <- "THERM.NOVEMBER.2010"
      else if (input$monthRight == "December")
        rightOption <- "THERM.DECEMBER.2010"
      else if (input$monthRight == "Total")
        rightOption <- "TOTAL.THERMS" 
    }
    else if (input$propRight == "Building Age")
    {
      rightOption <- "AVERAGE.BUILDING.AGE"
    }
    else if (input$propRight == "Building Height")
    {
      rightOption <- "AVERAGE.STORIES"
    }
    else if (input$propRight == "Total Population")
    {
      rightOption <- "TOTAL.POPULATION"
    }
    return(rightOption)
  })
  
  ### ------- the right side reactive functions ------- ###
  rightSideReactive <- reactive({
    if(input$typeRight == "All")
    {
      rightSide <- subset(energyUse2010, 
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaRight)
      rightSidev2 <- rightSide %>% select(GEOID, COMMUNITY.AREA.NAME,
                                          propSelectRight())
      rightSidev2 <- decen %>% inner_join(rightSidev2, "GEOID")
    }
    else if (input$typeRight == "Residential")
    {
      rightSide <- subset(energyUse2010, 
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaRight
                         & energyUse2010$BUILDING.TYPE == "Residential")
      rightSidev2 <- rightSide %>% select(GEOID, COMMUNITY.AREA.NAME,
                                        BUILDING.TYPE, propSelectRight())
      rightSidev2 <- decen %>% inner_join(rightSidev2, "GEOID")
    }
    else if (input$typeRight == "Commercial")
    {
      rightSide <- subset(energyUse2010, 
                          energyUse2010$COMMUNITY.AREA.NAME == input$areaRight
                          & energyUse2010$BUILDING.TYPE == "Commercial")
      rightSidev2 <- rightSide %>% select(GEOID, COMMUNITY.AREA.NAME,
                                          BUILDING.TYPE, propSelectRight())
      rightSidev2 <- decen %>% inner_join(rightSidev2, "GEOID")
    }
    else if (input$typeRight == "Industrial")
    {
      rightSide <- subset(energyUse2010, 
                          energyUse2010$COMMUNITY.AREA.NAME == input$areaRight
                          & energyUse2010$BUILDING.TYPE == "Industrial")
      rightSidev2 <- rightSide %>% select(GEOID, COMMUNITY.AREA.NAME,
                                          BUILDING.TYPE, propSelectRight())
      rightSidev2 <- decen %>% inner_join(rightSidev2, "GEOID")
    }
  })
  
  rightSideReactive2 <- reactive({
    if (input$propGTRight == "Electricity")
    {
      rightSide <- subset(energyUse2010,
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaRight)
      return(rightSide[5:16])
    }
    else if (input$propGTRight == "Gas")
    {
      rightSide <- subset(energyUse2010,
                         energyUse2010$COMMUNITY.AREA.NAME == input$areaRight)
      return(rightSide[20:31])
    }
  })
  
  ### -------- Reactive functions for the whole city of Chicago ------- ###
  totalReactiveLeft <- reactive({
    if (input$areaTotalLeft != "All")
    {
      energyData <- subset(energyUse2010, 
                           energyUse2010$COMMUNITY.AREA.NAME == input$areaTotalLeft)
      energyDatav2 <- energyData %>% select(GEOID, COMMUNITY.AREA.NAME, TOTAL.KWH)
      energyDatav2 <- decen %>% inner_join(energyDatav2, "GEOID")
    }
    else if (input$areaTotalLeft == "All")
    {
      if(input$levelLeft == "Block")
      {
        energyData <- energyUse2010 %>% select(GEOID, COMMUNITY.AREA.NAME, TOTAL.KWH)
        energyData <- decen %>% inner_join(energyData,"GEOID")
      }
      else if (input$levelLeft == "Tract")
      {
        energyData <- energyUse2010 %>% select(GEOID, COMMUNITY.AREA.NAME, TOTAL.KWH)
        energyData <- tract %>% inner_join(energyData, "GEOID")
      }
    }
  })
  
  ### -------- rendering the west side map ---------- ###
  output$wsMap <- renderLeaflet({
    wsData <- westSideReactive()
    mWS <- mapview(wsData, zcol = propertySelect())
    mWS@map
  })
  
  output$wsPlot <- renderPlot({
    westSidePlot <- westSideReactive2()
    sumdata<-data.frame(value=apply(westSidePlot, 2, sum))
    sumdata$key=rownames(sumdata)
    if (input$propGT == "Electricity")
    {
      ggplot(data=sumdata, aes(x=key,y=value,fill=key)) +
        geom_bar(colour="black", stat = "identity") +
        scale_x_discrete(name = "Month",
                         labels = c("KWH.APRIL.2010" = "April",
                                    "KWH.AUGUST.2010" = "August",
                                    "KWH.DECEMBER.2010" = "December",
                                    "KWH.FEBRUARY.2010" = "February",
                                    "KWH.JANUARY.2010" = "January",
                                    "KWH.JULY.2010" = "July",
                                    "KWH.JUNE.2010" = "June",
                                    "KWH.MARCH.2010" = "March",
                                    "KWH.MAY.2010" = "May",
                                    "KWH.NOVEMBER.2010" = "November",
                                    "KWH.OCTOBER.2010" = "October",
                                    "KWH.SEPTEMBER.2010" = "September"),
                         limits = c("KWH.JANUARY.2010", "KWH.FEBRUARY.2010",
                                    "KWH.MARCH.2010", "KWH.APRIL.2010", 
                                    "KWH.MAY.2010", "KWH.JUNE.2010",
                                    "KWH.JULY.2010", "KWH.AUGUST.2010",
                                    "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                                    "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010")) +
        scale_y_continuous(name = "Amount of Electricity (kWh)", labels = scales::comma)
    }
    else if (input$propGT == "Gas")
    {
      ggplot(data=sumdata, aes(x=key,y=value,fill=key)) +
        geom_bar(colour="black", stat = "identity") +
        scale_x_discrete(name = "Month",
                         labels = c("TERM.APRIL.2010" = "April",
                                    "THERM.AUGUST.2010" = "August",
                                    "THERM.DECEMBER.2010" = "December",
                                    "THERM.FEBRUARY.2010" = "February",
                                    "THERM.JANUARY.2010" = "January",
                                    "THERM.JULY.2010" = "July",
                                    "THERM.JUNE.2010" = "June",
                                    "THERM.MARCH.2010" = "March",
                                    "THERM.MAY.2010" = "May",
                                    "THERM.NOVEMBER.2010" = "November",
                                    "THERM.OCTOBER.2010" = "October",
                                    "THERM.SEPTEMBER.2010" = "September"),
                         limits = c("THERM.JANUARY.2010", "THERM.FEBRUARY.2010",
                                    "THERM.MARCH.2010", "TERM.APRIL.2010", 
                                    "THERM.MAY.2010", "THERM.JUNE.2010",
                                    "THERM.JULY.2010", "THERM.AUGUST.2010",
                                    "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                    "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")) +
        scale_y_continuous(name = "Amount of Thermal Energy", labels = scales::comma) + 
        ggtitle("Thermal Energy used in the West Side area") +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  output$wsTable <- DT::renderDataTable({
    DT::datatable({
      westSideDT <- westSideReactive2()
    },
    options = list(scrollX = TRUE))
  })
  
  ### render the left map for comparison
  output$leftMap <- renderLeaflet({
    leftData <- leftSideReactive()
    mL <- mapview(leftData, zcol = propSelectLeft())
    mL@map
  })
  
  output$leftPlot <- renderPlot({
    leftSidePlot <- leftSideReactive2()
    sumdata<-data.frame(value=apply(leftSidePlot, 2, sum))
    sumdata$key=rownames(sumdata)
    if (input$propGTLeft == "Electricity")
    {
      ggplot(data=sumdata, aes(x=key,y=value,fill=key)) +
        geom_bar(colour="black", stat = "identity") +
        scale_x_discrete(name = "Month",
                         labels = c("KWH.APRIL.2010" = "April",
                                    "KWH.AUGUST.2010" = "August",
                                    "KWH.DECEMBER.2010" = "December",
                                    "KWH.FEBRUARY.2010" = "February",
                                    "KWH.JANUARY.2010" = "January",
                                    "KWH.JULY.2010" = "July",
                                    "KWH.JUNE.2010" = "June",
                                    "KWH.MARCH.2010" = "March",
                                    "KWH.MAY.2010" = "May",
                                    "KWH.NOVEMBER.2010" = "November",
                                    "KWH.OCTOBER.2010" = "October",
                                    "KWH.SEPTEMBER.2010" = "September"),
                         limits = c("KWH.JANUARY.2010", "KWH.FEBRUARY.2010",
                                    "KWH.MARCH.2010", "KWH.APRIL.2010", 
                                    "KWH.MAY.2010", "KWH.JUNE.2010",
                                    "KWH.JULY.2010", "KWH.AUGUST.2010",
                                    "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                                    "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010")) +
        ggtitle(paste("Amount of Electricity used in the", input$areaLeft, 
                      "area (2010)")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(name = "Amount of Electricity (kWh)", labels = scales::comma)
    }
    else if (input$propGTLeft == "Gas")
    {
      ggplot(data=sumdata, aes(x=key,y=value,fill=key)) +
        geom_bar(colour="black", stat = "identity") +
        scale_x_discrete(name = "Month",
                         labels = c("TERM.APRIL.2010" = "April",
                                    "THERM.AUGUST.2010" = "August",
                                    "THERM.DECEMBER.2010" = "December",
                                    "THERM.FEBRUARY.2010" = "February",
                                    "THERM.JANUARY.2010" = "January",
                                    "THERM.JULY.2010" = "July",
                                    "THERM.JUNE.2010" = "June",
                                    "THERM.MARCH.2010" = "March",
                                    "THERM.MAY.2010" = "May",
                                    "THERM.NOVEMBER.2010" = "November",
                                    "THERM.OCTOBER.2010" = "October",
                                    "THERM.SEPTEMBER.2010" = "September"),
                         limits = c("THERM.JANUARY.2010", "THERM.FEBRUARY.2010",
                                    "THERM.MARCH.2010", "TERM.APRIL.2010", 
                                    "THERM.MAY.2010", "THERM.JUNE.2010",
                                    "THERM.JULY.2010", "THERM.AUGUST.2010",
                                    "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                    "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")) +
        ggtitle(paste("Thermal Energy used in the", input$areaLeft, 
                      "area (2010)")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(name = "Amount of Thermal Energy", labels = scales::comma)
    }
  })
  
  output$leftTable <- DT::renderDataTable({
    DT::datatable({
      leftDT <- leftSideReactive2()
    },
    options = list(scrollX = TRUE))
  })
  
  ### render the right map for comparison
  output$rightMap <- renderLeaflet({
    rightData <- rightSideReactive()
    mR <- mapview(rightData, zcol = propSelectRight())
    mR@map
  })
  
  output$rightPlot <- renderPlot({
    rightSidePlot <- rightSideReactive2()
    sumdata<-data.frame(value=apply(rightSidePlot, 2, sum))
    sumdata$key=rownames(sumdata)
    if (input$propGTRight == "Electricity")
    {
      ggplot(data=sumdata, aes(x=key,y=value,fill=key)) +
        geom_bar(colour="black", stat = "identity") +
        scale_x_discrete(name = "Month",
                         labels = c("KWH.APRIL.2010" = "April",
                                    "KWH.AUGUST.2010" = "August",
                                    "KWH.DECEMBER.2010" = "December",
                                    "KWH.FEBRUARY.2010" = "February",
                                    "KWH.JANUARY.2010" = "January",
                                    "KWH.JULY.2010" = "July",
                                    "KWH.JUNE.2010" = "June",
                                    "KWH.MARCH.2010" = "March",
                                    "KWH.MAY.2010" = "May",
                                    "KWH.NOVEMBER.2010" = "November",
                                    "KWH.OCTOBER.2010" = "October",
                                    "KWH.SEPTEMBER.2010" = "September"),
                         limits = c("KWH.JANUARY.2010", "KWH.FEBRUARY.2010",
                                    "KWH.MARCH.2010", "KWH.APRIL.2010", 
                                    "KWH.MAY.2010", "KWH.JUNE.2010",
                                    "KWH.JULY.2010", "KWH.AUGUST.2010",
                                    "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010",
                                    "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010")) +
        ggtitle(paste("Amount of Electricity used in the", input$areaRight, 
                      "area (2010)")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(name = "Amount of Electricity (kWh)", labels = scales::comma)
    }
    else if (input$propGTRight == "Gas")
    {
      ggplot(data=sumdata, aes(x=key,y=value,fill=key)) +
        geom_bar(colour="black", stat = "identity") +
        scale_x_discrete(name = "Month",
                         labels = c("TERM.APRIL.2010" = "April",
                                    "THERM.AUGUST.2010" = "August",
                                    "THERM.DECEMBER.2010" = "December",
                                    "THERM.FEBRUARY.2010" = "February",
                                    "THERM.JANUARY.2010" = "January",
                                    "THERM.JULY.2010" = "July",
                                    "THERM.JUNE.2010" = "June",
                                    "THERM.MARCH.2010" = "March",
                                    "THERM.MAY.2010" = "May",
                                    "THERM.NOVEMBER.2010" = "November",
                                    "THERM.OCTOBER.2010" = "October",
                                    "THERM.SEPTEMBER.2010" = "September"),
                         limits = c("THERM.JANUARY.2010", "THERM.FEBRUARY.2010",
                                    "THERM.MARCH.2010", "TERM.APRIL.2010", 
                                    "THERM.MAY.2010", "THERM.JUNE.2010",
                                    "THERM.JULY.2010", "THERM.AUGUST.2010",
                                    "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010",
                                    "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010")) +
        ggtitle(paste("Thermal Energy generated in the", input$areaRight, 
                      "area (2010)")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(name = "Amount of Thermal Energy", labels = scales::comma)
    }
  })
  
  output$rightTable <- DT::renderDataTable({
    DT::datatable({
      rightDT <- rightSideReactive2()
    },
    options = list(scrollX = TRUE))
  })
  
  ### render including the whole city of chicago
  output$totalMapLeft <- renderLeaflet({
    totalLeftData <- totalReactiveLeft()
    mTL <- mapview(totalLeftData, zcol = "TOTAL.KWH")
    mTL@map
  })
}

shinyApp(ui = ui, server = server)