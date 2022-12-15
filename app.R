# Tasks by each team member:
# Data cleaning and prep == ALL
# UI == ALL
# Application design and consistency of architecture == Alexa
# Tab 1 == Cole
# Tab 2 == Trevor
# Tab 3 == Corin, Alexa, & Trevor
# Presentation == Alexa & Trevor

# packages needed
library(plotly)
library(shiny)
library(ggplot2)
library(viridis)
library(shinydashboard)
library(rsconnect)
library(tidyverse)
library(sf)
library(leaflet)
library(shinyWidgets)
library(viridis)

# adjustment for plots
theme_update(plot.title = element_text(hjust = 0.5))

########### data loading ############# 
business <- read.csv("Business_Licenses_geocoded.csv")
parks <- read.csv("Parks_Locations_and_Features.csv")
facilities <- read.csv("Public_Facilities.csv")
districts <- st_read("City_Council_Districts.shp", stringsAsFactors = FALSE) 
census <- st_read("2020_CensusData.shp", stringsAsFactors = FALSE) 
calls <- read.csv("311_Phone_Call_Log_Mod.csv")
abandon <- st_read("Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE)
school <- st_read("School_Boundaries.shp", stringsAsFactors = FALSE)
parks.spatial <- read.csv('parks_spatial.csv')

########## data cleaning/prep ########## 

# filtering to active south bend businesses
business <- business %>% filter(City == "SOUTH BEND" & 
                                  License__1 == "Active and Licensed")
# df only containing restaurants
restaurants <- business %>% filter(Classifica %in% c("RESTAM", "RESTZ"))

# changing call date to date format
calls$Call_Date = as.Date(calls$Call_Date)

# filtering census population to only south bend using census 11-digit tract code
census <- st_as_sf(census)
census$GEOID <- substring(census$GEOID, 7)
census$GEOID <- paste(substr(census$GEOID, 1, 3), '.', substr(census$GEOID, 4, 5), sep = '')
census$GEOID <- as.numeric(census$GEOID)
census <- census %>%
  filter(GEOID <= 35 | GEOID %in% c(109, 110, 111, 112, 112.02, 113.01, 113.03, 117.02, 118, 118.01, 118.02, 119, 120))

# df for current abandoned properties only
abandoned <- abandon %>% filter(Code_Enfor != "In Compliance: Outcome Complete")

# df for tab 3 that includes only business types of interest
business.geocoded = business %>% filter(Classifica %in% c("RESTAM", "RESTZ", "AUTO", "HOTEL"))
# crearing new column with cleaned business type
business.geocoded$type <- NA
business.geocoded$type[business.geocoded$Classifica %in% c("RESTAM", "RESTZ")] <- "Restaurant"
business.geocoded$type[business.geocoded$Classifica == "AUTO"] <- "Automotive Repair"
business.geocoded$type[business.geocoded$Classifica == "HOTEL"] <- "Hotel"

# color pallete for the dashboard
v <- viridis(30)
colors <- c(v[6], v[12], v[18], v[24])

# function that enables custom value box colors
customValueBox <- function (value, subtitle, icon = NULL, color='white', background, width = 4, href = NULL){

  style <- paste0("color: ", color, "; background-color: ", background, ";")
  
  boxContent <- div(class = "small-box", style = style, 
                    div(class = "inner", h3(value), p(subtitle)), if (!is.null(icon)) 
                      div(class = "icon-large", icon))
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

########################## ui ########################## 
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "South Bend Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                        # creating side tabs
                        menuItem("Summary", tabName = "tab1", icon = icon("bar-chart")),
                        menuItem("Abandoned Properties", tabName = "tab2", icon = icon("map-marker-alt")),
                        menuItem("Local Businesses", tabName = "tab3", icon = icon("map-marker-alt"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        
                        tabItem(tabName = "tab1",
                                fluidRow(
                                  # key metric boxes
                                  valueBoxOutput("population"),
                                  valueBoxOutput("povertyPercent"),
                                  valueBoxOutput("medianincome"), 
                                  valueBoxOutput("localparks"),
                                  valueBoxOutput("localbusinesses"),
                                  valueBoxOutput("localrestaurants")
                                ),
                                # creating plot 1
                                fluidRow(align="center",
                                         plotlyOutput("businessLicenses",  width = "90%", height = "20vw")
                                ),
                                br(),
                                # creating plot 2
                                fluidRow(align="center",
                                         plotlyOutput("callVolume",  width = "90%", height = "20vw")
                                ),
                                br(),
                                # creating plot 3
                                fluidRow(align="center",
                                         plotlyOutput("DemoProp",  width = "90%", height = "20vw")
                                )),
                        
                        tabItem(tabName = "tab2",
                                fluidRow(
                                  # valueBox 1
                                  customValueBox(nrow(abandoned)-nrow(abandoned%>%filter(grepl('*Expired', Code_Enfor)))-nrow(abandoned%>%filter(grepl('*Demo', Code_Enfor))), "Properties with Planned Repairs", icon = icon("house"), background =colors[1]),
                                  # valueBox 2
                                  customValueBox(nrow(abandoned%>%filter(grepl('*Expired', Code_Enfor))), "Properties with Past Due Repairs", icon = icon("hammer"), background = colors[1]),
                                  # valueBox 3
                                  customValueBox(nrow(abandoned%>%filter(grepl('*Demo', Code_Enfor))), "Properties Planned for Demolition", icon = icon("house-circle-xmark"), background = colors[1]),
                                ),
                                fluidRow(
                                  #Map
                                  leafletOutput("mymap", height = 600))),
                        
                        tabItem(tabName = "tab3",
                                fluidRow(
                                # valueBox 1
                                customValueBox(nrow(business.geocoded%>%filter(Classifica %in% c("RESTAM", "RESTZ"))), "Restaurants", icon = icon("cutlery", lib="font-awesome"), background = v[12]),
                                # valueBox 2
                                customValueBox(nrow(business.geocoded%>%filter(Classifica == "AUTO")), "Auto Repair Shops", icon = icon("car", lib="font-awesome"), background = v[24]),
                                # valueBox 3
                                customValueBox(nrow(business.geocoded%>%filter(Classifica == "HOTEL")), "Hotels", icon = icon("hotel", lib="font-awesome"), background = v[30]),
                        ),
                        fluidRow(
                          # multi select input for business type
                          pickerInput(inputId = "types",
                                      label = "Business Type:",
                                      choices = unique(business.geocoded$type),
                                      selected = unique(business.geocoded$type),
                                      options = list('actions-box' = TRUE),
                                      multiple = TRUE),
                          #Map
                          leafletOutput("mymap2", height = 500)))
                      )
                    )
)

server <- function(input, output) {
  
  # require values for plot in tab 3
  result <- reactive({
    validate(
      need(input$types, "Please select a business type")
    )
  })
  
  ##################### Tab 1 ######################## 
  
  # value box 1
  output$population <- renderValueBox({
    customValueBox(prettyNum(sum(census$A00001_1), big.mark = ","), "Population", icon = icon("user"),
             background =colors[1])
  })
  
  # value box 2
  output$povertyPercent <- renderValueBox({
    customValueBox(
      paste0(round((sum(census$B13004_2) / sum(census$A00001_1) * 100), 1), "%"),
      "Population in Poverty", icon = icon("collapse-down", lib = "glyphicon"),
      background = colors[1]
    )
  })
  
  # value box 3
  output$localbusinesses <- renderValueBox({
    customValueBox(
      prettyNum(nrow(business), big.mark = ","), "Local Businesses", icon = icon("briefcase", lib = "glyphicon"),
      background = colors[1]
    )
  })
  
  # value box 4
  output$localrestaurants <- renderValueBox({
    customValueBox(
      prettyNum(length(unique(restaurants$Business_N)), big.mark = ","), "Unique Restaurants", icon = icon("cutlery", lib = "glyphicon"),
      background = colors[1]
    )
  })
  
  # value box 5
  output$localparks <- renderValueBox({
    customValueBox(
      nrow(parks), "Local Parks", icon = icon("tree-deciduous", lib = "glyphicon"),
      background = colors[1]
    )
  })
  
  # value box 6
  output$medianincome <- renderValueBox({
    customValueBox(
      paste0("$", prettyNum(round(mean(census$A14006_1)), big.mark = ",")), "Median Income", icon = icon("usd", lib = "glyphicon"),
      background = colors[1]
    )
  })
  
  # business licenses plot
  output$businessLicenses <- renderPlotly({
    
    business$Issue_Date = as.Date(business$Issue_Date)
    x <- ggplot(business, aes(x=Issue_Date)) +
      geom_histogram(bins=15, color="black", fill=colors[2]) + 
      labs(title = "New Businesses in South Bend",
           y= "Business Licenses Issued", x = "Date")
    ggplotly(x, tooltip = 'count')
    
  })
  
  # call volume plot
  output$callVolume <- renderPlotly({
    
    y <- ggplot(calls, aes(x=Call_Date)) +
      geom_histogram(bins=30, color="black", fill=colors[3]) + 
      labs(title = "311 Call Volume",
           y= "Number of Calls", x = "Date of Call")
    ggplotly(y, tooltip = 'count')
    
  })
  
  # demolished properites plot
  output$DemoProp <- renderPlotly({
    
    abandon$Date_of_Ou = as.Date(abandon$Date_of_Ou)
    z <- ggplot(abandon, aes(x=Date_of_Ou)) +
      geom_histogram(bins=15, color="black", fill=colors[4]) + 
      labs(title = "Abandoned Properties Demolished",
           y= "Number of Properties", x = "Date of Demolition")
    ggplotly(z, tooltip = 'count')
  })
  
  ################ Tab 2 ############################ 
  
  output$mymap <- renderLeaflet({leaflet()  %>%
      addTiles() %>%
      addPolygons(data=abandoned, popup = ~paste("Status:", Code_Enfor, "<br>Structure Type:", Structures), color = 'black') %>%
      # Overlay groups
      addPolygons(data = school,
                  popup = ~paste("School Name:", School),
                  stroke = FALSE, fillOpacity = 0.7, opacity = 0.3, color = v[8], group = "Schools") %>%
      addCircleMarkers(data = parks.spatial,
                       lng = ~X,
                       lat = ~Y,
                       popup = ~paste("Park Name:", Park_Name, "<br>Park Type:", Park_Type),
                       stroke = FALSE, fillOpacity = 0.5, opacity = 0.3, color = v[24], group = "Parks") %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Schools", "Parks"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
  })
  
  ############ Tab 3 ############### 

  # color pallete definition
  pal <-  colorFactor(palette = c(v[12], v[24], v[30]), 
                levels = c("Restaurant", "Automotive Repair", "Hotel"))
  # popup definition
  business.geocoded$popup <- paste("<b>",business.geocoded$Business_N,"</b><br>",
                                   "Type: ",business.geocoded$type)
  # plot
  output$mymap2 <- renderLeaflet({
    # includes error catcher
    result()
    # filters to only types selected
    business.geocoded2 <- business.geocoded[business.geocoded$type %in% input$types,]
    # plot
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = business.geocoded2,
                 lng = ~X,
                 lat = ~Y,
                 color = ~pal(type),
                 popup = ~popup,
                 radius = 2,
                 fillOpacity = 1,
                 opacity = 1)
      
    })
  

  
}

# render app
shinyApp(ui, server)


