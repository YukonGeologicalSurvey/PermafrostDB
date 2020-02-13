#######################################################################################
#--------------# Geotechnical data viewing app #-----------------#
#######################################################################################

pft.dbconnect <- function(classPath, username, password) {
  require(RJDBC)
  require(DBI)
  
  jdbcDriver <- JDBC("oracle.jdbc.OracleDriver", classPath)
  
  con <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//199.247.170.218:1521/YGS50D", username, password)
  
  return(con)
}

# Connect to database
con <- pft.dbconnect(classPath="C:/ojdbc6.jar", username, password)

enableBookmarking("url")
#-------------------------------------------------------------------------------------
###################### FUNCTIONS #######################
########## f.link: Add link to locs ##########
f.link <- function(locs) {
  links <- c()
  for (i in locs$name) {
    #locsl <- locs[locs$name==i,]
    #link <- c(paste0("'<a href = \"/?a=b", "&loc=", i, "\"> See site data here </a>'"))
    link <- c(paste0("'<a href = \"/?_inputs_&loc=%22", i, "%22&loc-selectized=%22%22",
                     "&Navbar=%22Data%22\"> See site data here </a>'"))
    links <- c(links, link)
  }
  locsl <- cbind(locs, links)
  
  return(locsl)
}

########## pft.map: Create map of all locations ###########
pft.map <- function(loc) {
  
  # Create map
  leaflet(loc) %>%
    addProviderTiles('Esri.WorldTopoMap') %>% # More here: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(lng=loc$long, lat=loc$lat, 
                     popup=popupTable(f.link(loc), row.numbers=FALSE, feature.id=FALSE),
                     color = "#0097A9", opacity=1)
  #leaflet::addLegend("topright", pal = pal, values = ~permafrost)
}

########## current.loc: Reactive function for locs subset ###########
## Subsets the locs table to a single location
current.loc <- function(n){
  cloc <- locs[locs$name==n,]
  return(cloc)
}

########## f.soil_desc ##############################################
f.soil_desc <- function(loc) {
  site_id <- loc
  tab <- dbGetQuery(con, paste0("SELECT TOP_DEPTH, BOT_DEPTH, BOUNDARY, SOIL_DESC, ",
                                "CLASS, USC_CODE, COMMENTS ",
                                "FROM PF_SOIL_DESC ",
                                "WHERE SITE_ID = '", site_id, "' ",
                                "ORDER BY TOP_DEPTH"))
  tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  return(tab)
}

########## f.pf_desc ##############################################
f.pf_desc <- function(loc) {
  site_id <- loc
  tab <- dbGetQuery(con, paste0("SELECT TOP_DEPTH, BOT_DEPTH, TEMPERATURE, SURFACE_THAW, ",
                                "PERMAFROST_DESC, ICE_CODE, CLASS, PERCENT_ICE, COMMENTS ",
                                "FROM PF_PERMAFROST_DESC ",
                                "WHERE SITE_ID = '", site_id, "' ",
                                "ORDER BY TOP_DEPTH"))
  tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  return(tab)
}

########## f.surface_desc ##############################################
f.surface_desc <- function(loc) {
  site_id <- loc
  tab <- dbGetQuery(con, paste0("SELECT OBSERVATION_DATE, VEG_TYPE, VEG_HEIGHT, VEG_DENSITY, ",
                                "ORGANIC_THICKNESS, TOPOGRAPHY, DRAINAGE, SURFICIAL_GEOLOGY, ",
                                "TERRAIN, SNOW_DEPTH, SNOW_DENSITY, SNOW_THERMAL_CONDUCT, ",
                                "SLOPE_ANGLE, SLOPE_ASPECT, DISTURBANCE, ECOREGION ",
                                "FROM PF_SURFACE_DESC ",
                                "WHERE SITE_ID = '", site_id, "'"))
  tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  return(tab)
}
########## f.meta ##############################################
f.meta <- function(loc) {
  site_id <- loc
  tab <- dbGetQuery(con, paste0("SELECT SITE_ID, PROJECT_NAME, LOCATION_DESC, ELEVATION, ",
                                "HOLE_DEPTH, START_DATE, END_DATE, CONSULTANT, ",
                                "CLIENT, CONTRACTOR, EQUIPMENT, CORE_DIAMETER ",
                                "FLUSH, PLUNGE, AZIMUTH, PROJECT_ENGINEER, HOLE_TYPE ",
                                "ROCK_DEPTH, COORD_ACQ, GW_TABLE, COMMENTS ",
                                "FROM PF_LOCATIONS ",
                                "WHERE SITE_ID = '", site_id, "'"))
  tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  return(tab)
}

#-------------------------------------------------------------------------------------
#################### START UP #######################################################

# Load packages
library(shiny)
library(leaflet)
library(htmlTable)
library(mapview)
library(DBI)
library(plotKML)
library(leafpop)
library(raster)
library(rgdal)

# Get locations
locs <- dbGetQuery(con, "SELECT SITE_ID, ELEVATION, HOLE_DEPTH, START_DATE,
                   END_DATE, LATITUDE, LONGITUDE, PROJECT_NUMBER
                   FROM PF_LOCATIONS ORDER BY SITE_ID")
names(locs) <- c("name", "elevation", "hole_depth", "start_date", "end_date",
                 "lat", "long", "project_number") 

# Create all locations map
map <- pft.map(locs)

#-------------------------------------------------------------------------------------
###################### UI ##################################
# Define UI for application that draws a histogram
ui <- function(request){fluidPage(

    # Application title
    titlePanel("Yukon Geotechnical Data"),# Set colour of Navigation bar
    tags$style(HTML(" 
        .navbar { background-color: #F2A900;}
        .navbar-default .navbar-nav > li > a {color:white;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #d99700;}
        .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#d99700;}
                  ")),
    
    navbarPage(title = "", id = "Navbar",
               
               tabPanel("Map", 
                        leafletOutput("mymap", height='750'),
                        downloadButton("downloadLoc.csv", "Download locations CSV"),
                        downloadButton('downloadLoc.kml', "Download locations KML"),
                        downloadButton('downloadLoc.shp', "Download locations shapefile")
                        ),
               tabPanel("Data",
                        # Locations panel
                        fluidRow(
                          column(4, selectizeInput("loc", "Site:",
                                                choices=locs$name,
                                                selected="", 
                                                options = list(maxOptions=15000))),
                          column(8, leafletOutput("locmap", height='300'))
                        ),
                        # Setup tabs within 'Data'
                        tabsetPanel(type = 'tabs',

                                    tabPanel("Soil description",
                                             uiOutput("soil")),
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
                                             uiOutput("metadata"))#,
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
  ### Output
  ###=============================================================================
  
  ### Reactive output functions
  # Create current location table reactive to loc input
  currentLoc <- reactive({current.loc(input$loc)})
  # Create soil description table
  soil_desc_input <- reactive(f.soil_desc(currentLoc()))
  # Create permafrost description table
  permafrost_desc_input <- reactive(f.pf_desc(currentLoc()))
  # Create surface description table
  surface_desc_input <- reactive(f.surface_desc(currentLoc()))
  # Create surface description table
  metadata_input <- reactive(f.meta(currentLoc()))
  
  ### Outputs
  ## All locations map
  output$mymap <- renderLeaflet({
    map  
  })
  
  ## Data download
  output$downloadLoc.csv <- downloadHandler(
    filename = paste0("GeotechLocations_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(locs, file)
    }
  )
  output$downloadLoc.kml <- downloadHandler(
    filename = paste0("GeotechLocations_", Sys.Date(), ".kml"),
    content = function(file) {
      xy <- locs[, c("long", "lat")]
      shp <- SpatialPointsDataFrame(xy, locs, proj4string = CRS("+proj=longlat +datum=NAD83"))
      kml(shp, folder.name = "Geotech locations", file)
    }
  )
  
  # # Version 1 to download shapefile
  # output$downloadLoc.shp <- downloadHandler(
  #   filename = paste0("GeotechLocations_", Sys.Date(), ".zip"),
  #   content = function(file) {
  #     # Create coords
  #     xy <- locs[, c("long", "lat")]
  #     # Create spatial object
  #     shp <- SpatialPointsDataFrame(xy, locs, proj4string = CRS("+proj=longlat +datum=NAD83"))
  #     # Create temporary folder
  #     temp_shp <- tempdir()
  #     # Create shapefile
  #     writeOGR(shp, temp_shp, 'GeotechLocations', "ESRI Shapefile", overwrite_layer=TRUE)
  #     # zip shapefile files
  #     zip_file <- file.path(temp_shp, "GeotechLocations.zip") # construct path to file
  #     shp_files <- list.files(temp_shp, "GeotechLocations", full.names = TRUE) # character list of files in temp_shp
  #     #shp_files2 <- list.files(temp_shp, "GeotechLocations")
  #     #named <- paste0(shp_files2, ".zip")
  #     #mapply(zip, zipfile= named, files = shp_files)
  #     zip(zipfile= zip_file, files = shp_files)
  #     # copy the zip file to the file argument
  #     file.copy(zip_file, file)
  #     # remove all the files created
  #     file.remove(zip_file, shp_files)
  #   }
  # )
  
  ## Version 2 to download shapefile
  output$downloadLoc.shp <- downloadHandler(
    filename = function() {
      paste0("GeotechLocations_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create coords
      xy <- locs[, c("long", "lat")]
      # Create spatial object
      shp <- SpatialPointsDataFrame(xy, locs, proj4string = CRS("+proj=longlat +datum=NAD83"))
      # Create temporary folder
      tempdir <- tempdir()
      setwd(tempdir())
      # Create shapefile
      writeOGR(shp, tempdir, 'GeotechLocations', "ESRI Shapefile", overwrite_layer=TRUE)
      # zip shapefile files
      zip_file <- file.path(tempdir, "GeotechLocations.zip") # construct path to file
      shp_files <- list.files(tempdir, "GeotechLocations", full.names = FALSE) # character list of files in temp_shp
      zip(zipfile= file, files = shp_files)
    }
  )
  
    
  ## Single location map
  output$locmap <- renderLeaflet({
    cloc <- currentLoc()
    setView(map, lng=cloc$long, lat=cloc$lat, zoom=15)
  })
  
  ## Soil description
  output$soil <- renderUI({
    if(is.null(soil_desc_input()))
       return("No data available")
    tableOutput("soil_desc")
  })
  output$soil_desc <- renderTable({
    soil_desc_input()
  })
  
  ## Permafrost description
  output$permafrost <- renderUI({
    if(nrow(permafrost_desc_input())==0)
      return("No data available")
    tableOutput("permafrost_desc")
  })
  output$permafrost_desc <- renderTable({
    permafrost_desc_input()
  })

  ## Surface description
  output$surface <- renderUI({
    if(nrow(surface_desc_input())==0)
      return("No data available")
    tableOutput("surface_desc")
  })
  output$surface_desc <- renderTable({
    surface_desc_input()
  })
  
  ## Metadata
  output$surface <- renderUI({
    if(nrow(metadata_input())==0)
      return("No data available")
    tableOutput("metadata")
  })
  output$metadata <- renderTable({
    metadata_input()
  })

  ## Samples
  # Sample
  output$sample <- renderTable({
    site_id <- input$loc
    tab <- dbGetQuery(con, paste0("SELECT SAMPLE_NUMBER, TOP_DEPTH, BOT_DEPTH, CORE_DIA, ",
                                  "TYPE, USC_CODE, COMMENTS ",
                                  "FROM PF_SAMPLE ",
                                  "WHERE SITE_ID = '", site_id, "' ",
                                  "ORDER BY SAMPLE_NUMBER"))
    tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  }, caption = "Sample", caption.placement = getOption("xtable.caption.placement", "top"),)
  
  # Permafrost testing
  output$permafrost_testing <- renderTable({
    site_id <- input$loc
    tab <- dbGetQuery(con, paste0("SELECT SAMPLE_NUMBER, TOP_DEPTH, BOT_DEPTH, THAW_WEAKEN, ",
                                  "THAW_STRAIN, UNFROZEN_WATER, CREEP, ADFREEZE, THERMAL_COND, ",
                                  "LATENT_HEAT_FUSION, COMMENTS ",
                                  "FROM PF_PERMAFROST_TESTING ",
                                  "WHERE SITE_ID = '", site_id, "' ",
                                  "ORDER BY SAMPLE_NUMBER"))
    tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  }, caption = "Permafrost testing", caption.placement = getOption("xtable.caption.placement", "top"),)
  
  # Geotech testing
  output$geotech_testing <- renderTable({
    site_id <- input$loc
    tab <- dbGetQuery(con, paste0("SELECT SAMPLE_NUMBER, BULK_DENSITY, DRY_DENSITY, N_VALUE, ",
                                  "LPT_N, GS, MOISTURE, LL, PL, PI, GRAVEL, SAND, FINES, SILT, ",
                                  "CLAY, D50, ORGANICS, SOLUABLE_SULPH, SALINITY, TEST_COM, ",
                                  "TOP_DEPTH, COMMENTS ",
                                  "FROM PF_GEOTECH_TESTING ",
                                  "WHERE SITE_ID = '", site_id, "' ",
                                  "ORDER BY SAMPLE_NUMBER"))
    tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  }, caption = "Geotech testing", caption.placement = getOption("xtable.caption.placement", "top"),)
  
  # Environmental testing
  output$enviro_testing <- renderTable({
    site_id <- input$loc
    tab <- dbGetQuery(con, paste0("SELECT SAMPLE_NUMBER, TOP_DEPTH, BOT_DEPTH, HYDROCARBON, ",
                                  "LEL, PID, ELECTRICAL_CONDUCTIVITY, CHLORIDE, METHANE, COMMENTS ",
                                  "FROM PF_ENVIRONMENTAL ",
                                  "WHERE SITE_ID = '", site_id, "' ",
                                  "ORDER BY SAMPLE_NUMBER"))
    tab <- tab[,colSums(is.na(tab))<nrow(tab)]
  }, caption = "Environmental", caption.placement = getOption("xtable.caption.placement", "top"),)

}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
#-------------------------------------------------------------------------------------






















