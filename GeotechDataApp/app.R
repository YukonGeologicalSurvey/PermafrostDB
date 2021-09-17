#######################################################################################
#--------------# Geotechnical data viewing app #-----------------#
#######################################################################################

source('dbconnection.R')

enableBookmarking("url")
#-------------------------------------------------------------------------------------
###################### FUNCTIONS #######################

########## pft.map: Create map of all locations ###########
pft.map <- function(loc) {
  
  # Define f.link function
  # f.link <- function(tab) {
  #   
  #   linking <- function(i){
  #     link <- c(paste0("'<a href = \"?_inputs_&loc=%22", i, "%22&loc-selectized=%22%22",
  #                      "&Navbar=%22Data%22\"> See site data here </a>'"))
  #     return(link)
  #   }
  #   links <- sapply(tab$name, linking)
  #   
  #   tabl <- cbind(tab, links)
  #   
  #   return(tabl)
  # }
  
  # Create map
  leaflet(loc) %>%
    addProviderTiles('Esri.WorldTopoMap') %>% # More here: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
    addCircleMarkers(lng=loc$Longitude, lat=loc$Latitude, 
                     popup=popupTable(loc, row.numbers=FALSE, feature.id=FALSE),
                     color = "#800403", opacity=1)
}

########## f.soil ##############################################
f.soil <- function(loc) {
  site_id <- loc
  tab <- dbGetQuery(con, paste0("SELECT TOP_DEPTH, BOT_DEPTH, BOUNDARY, SOIL_DESC, ",
                                "CLASS, USC_CODE, COMMENTS ",
                                "FROM PERMAFROST.PF_SOIL_DESC ",
                                "WHERE SITE_ID = '", site_id, "' ",
                                "ORDER BY TOP_DEPTH"))
  names(tab) <- c("Top depth (m)", "Bottom depth (m)", "Boundary", "Soil description", "Class",
                  "USC code", "Comments")
  tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  return(tab)
}

########## f.permafrost ##############################################
f.permafrost <- function(loc) {
  site_id <- loc
  tab <- dbGetQuery(con, paste0("SELECT TOP_DEPTH, BOT_DEPTH, TEMPERATURE, SURFACE_THAW, ",
                                "PERMAFROST_DESC, ICE_CODE, CLASS, PERCENT_ICE, COMMENTS ",
                                "FROM PERMAFROST.PF_PERMAFROST_DESC ",
                                "WHERE SITE_ID = '", site_id, "' ",
                                "ORDER BY TOP_DEPTH"))
  names(tab) <- c("Top depth (m)", "Bottom depth (m)", "Temperature (\u00B0C)", "Surface thaw",
                  "Permafrost description", "Ice code", "Class", "Percent ice", "Comments")
  tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  return(tab)
}

########## f.surface ##############################################
f.surface <- function(loc) {
  site_id <- loc
  tab <- dbGetQuery(con, paste0("SELECT OBSERVATION_DATE, VEG_TYPE, VEG_HEIGHT, VEG_DENSITY, ",
                                "ORGANIC_THICKNESS, TOPOGRAPHY, DRAINAGE, SURFICIAL_GEOLOGY, ",
                                "TERRAIN, SNOW_DEPTH, SNOW_DENSITY, SNOW_THERMAL_CONDUCT, ",
                                "SLOPE_ANGLE, SLOPE_ASPECT, DISTURBANCE, ECOREGION ",
                                "FROM PERMAFROST.PF_SURFACE_DESC ",
                                "WHERE SITE_ID = '", site_id, "'"))
  names(tab) <- c("Observation date", "Vegetation type", "Vegetation height", "Vegetation density",
                  "Organic thickness", "Topography", "Drainage", "Surficial geology", "Terrain",
                  "Snow depth", "Snow density", "Snow thermal conductivity", "Slope angle",
                  "Slope aspect", "Disturbance", "Ecoregion")
  tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  return(tab)
}
########## f.meta ##############################################
f.meta <- function(loc) {
  site_id <- loc
  tab <- dbGetQuery(con, paste0("SELECT SITE_ID, PROJECT_NAME, LOCATION_DESC, ELEVATION, ",
                                "HOLE_DEPTH, START_DATE, END_DATE, CONSULTANT, ",
                                "CLIENT, CONTRACTOR, EQUIPMENT, CORE_DIAMETER, ",
                                "FLUSH, PLUNGE, AZIMUTH, PROJECT_ENGINEER, HOLE_TYPE, ",
                                "ROCK_DEPTH, GW_TABLE, COMMENTS ",
                                "FROM PERMAFROST.PF_LOCATIONS ",
                                "WHERE SITE_ID = '", site_id, "'",
                                " AND PUBLIC_FLAG = 'Y'"))
  names(tab) <- c("Site id", "Project name", "Location description", "Elevation (m)", "Hole depth (m)",
                  "Start date", "End date", "Consultant", "Client", "Contractor", "Equipment",
                  "Core diameter", "Flush", "Plunge", "Azimuth", "Project engineer", "Hole type",
                  "Rock depth (m)", "Groundwater table", "Comments")
  tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  return(tab)
}

########## filter.locs: Reactive function for locs filtered by string ###########

## Filters the locs table to the names with string
filter.locs <- function(s){
  flocs <- locs[grep(s, locs$Name, ignore.case=TRUE),]
  return(flocs)
}

#-------------------------------------------------------------------------------------
#################### START UP #######################################################

# Load packages
library(shiny)
library(leaflet)
library(htmlTable)
library(DBI)
library(plotKML)
library(leafpop)
library(raster)
library(rgdal)
library(shinycssloaders)

# Get locations
locs <- dbGetQuery(con, "SELECT SITE_ID, ELEVATION, HOLE_DEPTH, START_DATE,
                   END_DATE, LATITUDE, LONGITUDE, PROJECT_NUMBER, URL
                   FROM PERMAFROST.V_PF_LOCATIONS_MAP ORDER BY SITE_ID")
names(locs) <- c("Name", "Elevation", "Hole depth (m)", "Start date", "End date",
                 "Latitude", "Longitude", "Project number", "Link") 

# Create all locations map
#map <- pft.map(locs)

#-------------------------------------------------------------------------------------
###################### UI ##################################
# Define UI for application that draws a histogram
ui <- function(request){fluidPage(
  
  tags$style(HTML(" 
        .navbar { background-color: #F2A900;}
        .navbar-default .navbar-nav > li > a {color:white;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #d99700;}
        .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#d99700;}
        #mymap {height: calc(100vh - 250px) !important; 
                  ")),
  
  navbarPage(title = "", id = "Navbar",
             
             tabPanel("Map", 
                      textInput("string", "Location filter/search:", 
                                placeholder = "Search any string to filter locations",
                                value = ""),
                      leafletOutput("mymap") %>% 
                        withSpinner(color="#0097A9")
             ),
             tabPanel("Data",
                      # Locations panel
                      fluidRow(
                        column(4, selectizeInput("loc", "Site:",
                                                 choices=locs$Name,
                                                 selected="", 
                                                 options = list(maxOptions=15000))),
                        column(8, leafletOutput("locmap", height='300') %>% 
                                 withSpinner(color="#0097A9"))
                      ),
                      # Setup tabs within 'Data'
                      tabsetPanel(id="tabs", type = 'tabs',
                                  
                                  tabPanel("Soil description",
                                           uiOutput("soil") %>% 
                                             withSpinner(color="#0097A9")),
                                  tabPanel("Permafrost description",
                                           uiOutput("permafrost")),
                                  # tabPanel("Surface description",
                                  #          uiOutput("surface")),
                                  tabPanel("Samples",
                                           fluidPage(
                                             fluidRow(
                                               column(12, tableOutput("sample"))
                                             ),
                                             fluidRow(column(12, tableOutput("permafrost_testing"))
                                             ),
                                             fluidRow(column(12, tableOutput("geotech_testing"))
                                             ),
                                             fluidRow(column(12, tableOutput("enviro_testing")))
                                           )),
                                  tabPanel("Metadata",
                                           uiOutput("meta"))#,
                                  # tabPanel("Install",
                                  #          fluidRow(
                                  #            column(6, tableOutput("install_desc")),
                                  #            column(6, tableOutput("backfill"))
                                  #          )
                                  # )
                      ))
  )
)
}

#-------------------------------------------------------------------------------------
#################### Server ####################################
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Update browsers location bar every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url){
    updateQueryString(url)
  })
  
  ###=============================================================================
  ### Selection
  ###=============================================================================
  # Locations selection
  output$firstSelection <- renderUI({
    # Set select input
    selectInput("loc",
                label = NULL,
                choices=  locs$Name,  #"YGS_TakhiniValley",
                selected=NULL)
  })
  
  ###=============================================================================
  ### Output
  ###=============================================================================
  
  ### Reactive output functions
  # Create locations table reactive to string input
  filteredLoc <- reactive({filter.locs(input$string)})
  # Create soil description table
  soil_input <- reactive(f.soil(input$loc))
  # Create permafrost description table
  permafrost_input <- reactive(f.permafrost(input$loc))
  # Create surface description table
  surface_input <- reactive(f.surface(input$loc))
  # Create surface description table
  meta_input <- reactive(f.meta(input$loc))
  # Map so it can be used for the single location map
  currentMap <- reactive({pft.map(filteredLoc())})
  
  ### Outputs
  ## All locations map
  output$mymap <- renderLeaflet({
    #map 
    currentMap()
  })
  
  
  ## Single location map
  output$locmap <- renderLeaflet({
    cloc <- locs[locs$Name==input$loc,]
    setView(currentMap(), lng=cloc$Longitude, lat=cloc$Latitude, zoom=15)
  })
  
  ## Soil description
  output$soil <- renderUI({
    if(nrow(soil_input())==0)
      return("No data available")
    tableOutput("soil_desc")
  })
  output$soil_desc <- renderTable({
    soil_input()
  })
  
  # Reactive expression to hide tab
  tab_pf <- reactive({
    if (nrow(permafrost_input())==0){
      tab_pf <- "hide"
    } else {tab_pf <- "show"}
  })
  observe({
    if (tab_pf()=="hide") {
      hideTab("tabs", "Permafrost description")
    } else if (tab_pf()=="show") {
      showTab("tabs", "Permafrost description")
    }
  }) 
  
  ## Permafrost description
  output$permafrost <- renderUI({
    #if(nrow(permafrost_input())==0) {
    #   hideTab('tabs', "Permafrost description")
    # return("No data available")
    #}
    #else { 
    tableOutput("permafrost_desc") #}
  })
  output$permafrost_desc <- renderTable({
    permafrost_input()
  })
  
  ## Surface description
  output$surface <- renderUI({
    if(nrow(surface_input())==0)
      return("No data available")
    tableOutput("surface_desc")
  })
  output$surface_desc <- renderTable({
    surface_input()
  })
  
  ## Metadata
  output$meta <- renderUI({
    if(nrow(meta_input())==0)
      return("No data available")
    tableOutput("metadata")
  })
  output$metadata <- renderTable({
    meta_input()
  })
  
  ## Samples
  # Sample
  output$sample <- renderTable({
    site_id <- input$loc
    tab <- dbGetQuery(con, paste0("SELECT SAMPLE_NUMBER, TOP_DEPTH, BOT_DEPTH, CORE_DIA, ",
                                  "TYPE, USC_CODE, COMMENTS ",
                                  "FROM PERMAFROST.PF_SAMPLE ",
                                  "WHERE SITE_ID = '", site_id, "' ",
                                  "ORDER BY TOP_DEPTH"))
    names(tab) <- c("Sample number", "Top depth (m)", "Bottom depth (m)", "Core diameter",
                    "Type", "USC code", "Comments")
    tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  }, caption = "Sample", caption.placement = getOption("xtable.caption.placement", "top"),)
  
  # Permafrost testing
  output$permafrost_testing <- renderTable({
    site_id <- input$loc
    tab <- dbGetQuery(con, paste0("SELECT SAMPLE_NUMBER, TOP_DEPTH, BOT_DEPTH, THAW_WEAKEN, ",
                                  "THAW_STRAIN, UNFROZEN_WATER, CREEP, ADFREEZE, THERMAL_COND, ",
                                  "LATENT_HEAT_FUSION, COMMENTS ",
                                  "FROM PERMAFROST.PF_PERMAFROST_TESTING ",
                                  "WHERE SITE_ID = '", site_id, "' ",
                                  "ORDER BY TOP_DEPTH"))
    names(tab) <- c("Sample number", "Top depth (m)", "Bottom depth (m)", "Thaw weakening susceptibility",
                    "Thaw strain consolidation", "Unfrozen water content", "Creep properties",
                    "Adfreeze strength", "Thermal conductivity", "Latent heat of fusion", "Comments")
    tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  }, caption = "Permafrost testing", caption.placement = getOption("xtable.caption.placement", "top"),)
  
  # Geotech testing
  output$geotech_testing <- renderTable({
    site_id <- input$loc
    tab <- dbGetQuery(con, paste0("SELECT SAMPLE_NUMBER, TOP_DEPTH, BULK_DENSITY, DRY_DENSITY, N_VALUE, ",
                                  "GS, MOISTURE, LL, PL, PI, GRAVEL, SAND, FINES, SILT, ",
                                  "CLAY, D50, ORGANICS, SOLUABLE_SULPH, SALINITY, ",
                                  "COMMENTS ",
                                  "FROM PERMAFROST.PF_GEOTECH_TESTING ",
                                  "WHERE SITE_ID = '", site_id, "' ",
                                  "ORDER BY TOP_DEPTH"))
    names(tab) <- c("Sample number", "Top depth (m)", "Bulk density(kg/m)",
                    "Dry density(kg/m)", "N value", "Specific gravity", "Moisture content (%)",
                    "Liquid limit", "Plastic limit", "Plasticity index", "Gravel (%)", "Sand (%)",
                    "Fines (%)", "Silt (%)", "Clay (%)", "D50 (mm)", "Organics (%)", "Soluble sulphates (%)",
                    "Salinity (%)", "Comments")
    tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  }, caption = "Geotech testing", caption.placement = getOption("xtable.caption.placement", "top"),)
  
  # Environmental testing
  output$enviro_testing <- renderTable({
    site_id <- input$loc
    tab <- dbGetQuery(con, paste0("SELECT SAMPLE_NUMBER, TOP_DEPTH, BOT_DEPTH, HYDROCARBON, ",
                                  "LEL, PID, ELECTRICAL_CONDUCTIVITY, CHLORIDE, METHANE, COMMENTS ",
                                  "FROM PERMAFROST.PF_ENVIRONMENTAL ",
                                  "WHERE SITE_ID = '", site_id, "' ",
                                  "ORDER BY TOP_DEPTH"))
    names(tab) <- c("Sample number", "Top depth (m)", "Bottom depth (m)", "Hydrocarbon vapour (ppm)",
                    "Lower explosive limit (%)", "PID (%)", "Electrical conductivity (dS/m)",
                    "Chloride content (mg/L)", "Methane content", "Comments")
    tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  }, caption = "Environmental", caption.placement = getOption("xtable.caption.placement", "top"),)
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
#-------------------------------------------------------------------------------------














