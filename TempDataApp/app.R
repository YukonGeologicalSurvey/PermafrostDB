#######################################################################################
#--------------------------# Temperature data Shiny app #-----------------------------#
#######################################################################################

source("dbconnection.R")

#-------------------------------------------------------------------------------------
#################### FUNCTIONS #######################################################
########## pft.map: Leaflet map ##########
pft.map <- function(loc) {
  
  # Function to add link to locs and to change field names
  f.link <- function(locs) {
    links <- c()
    for (i in locs$name) {
      link <- c(paste0("'<a href = \"?_inputs_&aggr=%22day%22&aggr-selectized=%22%22",
                       "&loc=%22", i, "%22&loc-selectized=%22%22",
                       "&Navbar=%22Data%22\"> See site data here </a>'"))
      links <- c(links, link)
    }
    locsl <- cbind(locs, links)
    # to make method and who faster
    # # remove who_id and method_id
    # locsl <- locsl[,!(names(locsl) %in% c("who_id","method_id"))]
    # Change field names
    colnames(locsl) <- c("Name", "Start year", "End year", "Min depth", "Max depth",
                         "Latitude", "Longitude", "Permafrost", "Link")
    
    return(locsl)
  }
  # Set up icon colours
  pal <- colorFactor(c("#DC4405", "grey40", "#512A44", "#0097A9"), domain = c("No", "Undetermined", "Yes", "Weather station"))
  # Create map
  leaflet(loc) %>%
    addProviderTiles('Esri.WorldTopoMap', group = "Topo map") %>% # 'Esri.WorldTopoMap''Esri.WorldImagery' More here: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
    addProviderTiles('Esri.WorldImagery', group = "Satellite imagery") %>%
    addCircleMarkers(lng = loc$long, lat = loc$lat,
                     popup = leafpop::popupTable(f.link(loc), row.numbers = FALSE, feature.id = FALSE),
                     label = loc$name,
                     color = ~pal(Permafrost), 
                     opacity = 1, 
                     fillOpacity = 0.8) %>%
    leaflet::addLegend("topright", pal = pal, values = ~Permafrost) %>%
    addLayersControl(
    baseGroups = c("Topo map", "Satellite imagery")
    ) %>%
    addScaleBar(
      position = "bottomright",
      options = scaleBarOptions(imperial=FALSE)
    )
  
}


########## pft.query and pft.subset: Reactive table and dygraph query and subset #################################

## Query daily average data from PFT_SUMMARY in database 
pft.query <- function(con, location) { 
  
  obs <- dbGetQuery(con, paste0("SELECT NAME, TEMPDATE, MONTH, YEAR, DEPTH, DAILYTEMP ",#,
                                #"WHO_ID ",# METHOD_ID ",
                                "FROM PERMAFROST.PFT_SUMMARY2 ",
                                "WHERE NAME = '", location, "' ",
                                "AND DEPTH <= 0 ",
                                "AND PUBLIC_FLAG = 'Y'",
                                "ORDER BY TEMPDATE, DEPTH DESC" ))
  obs$TEMPDATE <- as.Date(obs$TEMPDATE)
  obs$MONTH <- as.Date(obs$MONTH)
  #obs$YEAR <- as.Date(obs$YEAR)
  names(obs) <- c("name", "date", "month", "year", "depth", "temp")#, "who_id", "method_id")
  return(obs)
}

## Aggregate and subset data from pft.query
pft.subset <- function(obs, aggr, depth_min, depth_max, date_s, date_e) {
  
  if (aggr=="none") {
    location <- unique(obs$name)
    obs <- dbGetQuery(con, paste0("SELECT TEMPTIME, DEPTH, ROUND(TEMP, 2) AS TEMP ",
                                  "FROM PERMAFROST.PFT_SUMMARY ",
                                  "WHERE NAME = '", location, "' ",
                                  "AND DEPTH <= 0 ",
                                  "AND PUBLIC_FLAG = 'Y'",
                                  "ORDER BY TEMPTIME, DEPTH DESC"))
    obs$TEMPTIME <- as.POSIXct(obs$TEMPTIME, tz = "", format = "%Y-%m-%d %H:%M")
    names(obs) <- c("date", "depth", "temp")
    obs <- obs[obs$depth >= depth_max & obs$depth <= depth_min &
                 obs$date >= date_s & obs$date <= date_e,]
    
  } else if (aggr=="day") {
    obs <- obs[obs$depth >= depth_max & obs$depth <= depth_min &
                 obs$date >= date_s & obs$date <= date_e,
               c("date", "depth", "temp")]
    
  } else if (aggr=="month") {
    obs <- aggregate(obs, by=list(obs$month, obs$depth), FUN=mean)
    obs <- obs[obs$depth >= depth_max & obs$depth <= depth_min &
                 obs$month >= date_s & obs$month <= date_e, 
               c("date", "depth", "temp")]
    obs$temp <- round(obs$temp, digits=2)
    
  } else if (aggr=="year") {
    obs <- aggregate(obs, by=list(obs$year, obs$depth), FUN=mean)
    obs <- obs[obs$depth >= depth_max & obs$depth <= depth_min &
                 obs$year >= date_s & obs$year <= date_e,
               c("date", "depth", "temp")]
    obs$temp <- round(obs$temp, digits=2)
  }
  
  return(obs)
  
}

########## pft.plot: Time series plotting function ####################################

## Plot the data from obs in a dygraph time series graph
pft.plot <- function(obs) {
  
  require(dygraphs)
  require(xts)
  require(viridis)
  
  date <- data.frame(date=unique(obs$date))
  
  depths <- sort(unique(obs$depth), decreasing = TRUE)
  series <- NULL
  snames <- NULL
  
  for (d in 1:length(depths)) {
    # match the values to times with merge to get regular series
    y <- subset(obs, depth == depths[d], select=c(temp, date))
    m <- merge(x = date, y = y, by = "date", all.x = TRUE)
    #only take columns with values
    if (sum(is.na(m$temp) == 0) > 0) {
      series <- cbind(series, m$temp)
      snames <- c(snames, paste(depths[d], "m"))	
    } 
  }  
  # Make time series
  qxts <- xts(series, order.by=date$date)
  
  # get number of time series
  snamesLen <- length(snames)
  
  #plot time series
  graph <- dygraph(qxts, ylab = "temperature (\u00B0C)", xlab = "date")
  
  # iterate to create series labels
  for (seriesNum in 1:snamesLen)
  {
    graph <- graph %>% dySeries(paste0("V",seriesNum) 
                                ,label = snames[seriesNum])
  }
  
  # Create plot
  graph <- graph %>%
    dyLegend(width = 100, labelsSeparateLines = TRUE, labelsDiv = 'legend') %>%
    dyOptions(labelsUTC = TRUE, gridLineWidth = 0.1, 
              colors = colorRampPalette(c("#DC4405", "#F2A900", "#7A9A01", "#0097A9"))(snamesLen)
    ) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
    dyLimit(limit = 0, strokePattern = "solid")
  
  # display graph
  graph 
  
}

########## pft.format: Table formatting function ########################################

## Formatting obs table from long to wide
pft.tformat <- function(table) {
  table <- reshape(table, idvar = 'date', v.names = 'temp', timevar = 'depth', direction = "wide")
  table$date <- as.character(table$date)
  colnames(table) <- gsub(pattern="temp.", replacement="", x=colnames(table))
  table <- table[, c("date", sort(as.numeric(colnames(table)[-1]), decreasing=TRUE))]
  colnames(table) <- c("Date", paste(colnames(table)[-1], " m"))
  
  return(table)
}

########## pft.plot_groundtempenv: Ground temperature envelope plotting function ##################################

## Plot ground temperature envelope for all years between date_s and date_e 
pft.plot_groundtempenv <- function(obs, date_s, date_e) { 
  
  # Subset dataframe
  obs <- obs[obs$date >= as.Date(date_s) & obs$date <= as.Date(date_e) & obs$depth <= 0,]
  
  # Calculate min, max, mean and count by year and depth
  min <- aggregate(obs, by=list(obs$year, obs$depth), FUN=min)
  max <- aggregate(obs, by=list(obs$year, obs$depth), FUN=max)
  mean <- aggregate(obs, by=list(obs$year, obs$depth), FUN=mean)
  counts <- count(obs, year, depth)
  
  obs <- cbind(min[,c("depth", "year", "temp")], max[,"temp"], mean[, "temp"])
  obs <- merge(obs, counts, by = c("depth", "year"))
  obs$year <- gsub("-01-01", "", obs$year)
  
  # Set col names
  names(obs) <- c("depth", "year", "min", "max", "mean", "count")
  
  years <- NULL
  annot <- NULL
  
  for (i in unique(obs$year)) {
    mn <- min(obs[obs$year==i,]$count)
    
    if (mn < 350) {
      anno <- NULL
      anno <- paste0("Incomplete dataset")
    } else {anno <- NA}
    
    annot <- c(annot, anno)
    years <- c(years, i)
  }
  
  annots <- data.frame(year=years, annot=annot)
  
  # ggplot2 plot
  library(ggplot2)
  
  plt <- ggplot(obs) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), plot.title = element_text(hjust=0.5),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          strip.background = element_rect(fill="#7A9A01"),
          strip.text = element_text(colour = 'white')) +
    coord_flip() +
    labs(x="depth (m)", y="temperature (\u00B0C)") +
    geom_hline(yintercept = 0, linetype="dashed", color="grey") +    # vertical line at 0
    geom_vline(xintercept = 0, color="grey") +    # Horizontal line at 0
    
    geom_line(aes(x=depth, y=max), color="#DC4405", size=0.75) +     # Maximum line
    geom_point(aes(x=depth, y=max), color="#DC4405", size=2) + # firebrick2
    
    geom_line(aes(x=depth, y=min), color="#0097A9", size=0.75) +     # Minimum line
    geom_point(aes(x=depth, y=min), color="#0097A9", size=2) + # steelblue3
    
    geom_line(aes(x=depth, y=mean), color="grey20", size=0.75) +     # Mean line
    geom_point(aes(x=depth, y=mean), color="grey20", size=2) + 
    
    geom_text(data=annots, aes(
      x=min(obs$depth), y=max(obs$max),
      hjust=1, vjust=0.3,# Add incomplete dataset text
      label=annot, col="#F2A900"),
      show.legend=FALSE) +
    facet_wrap(~year) 
  
  print(plt)
  
}


########## filter.locs: Reactive function for locs filtered by string ###########

## Filters the locs table to the names with string
filter.locs <- function(s){
  flocs <- locs[grep(s, locs$name, ignore.case=TRUE),]
  # to make method and who faster
  # flocs <- flocs[,!(names(flocs) %in% c("who_id", "method_id"))]
  return(flocs)
}

########## current.loc: Reactive function for locs subset ###########

## Subsets the locs table to a single location
current.loc <- function(n){
  cloc <- locs[locs$name==n,]
  # to make method and who faster
  # cloc <- cloc[,!(names(cloc) %in% c("who_id", "method_id"))]
  return(cloc)
}

########## location.met, who.met, method.met: Meta data tables functions ###############
## Metadata
# Location
location.met <- function(loc) {
  tab <- dbGetQuery(con, paste0("SELECT ID, NAME, LATITUDE, LONGITUDE, ELEVATION, LOCATION_ACCURACY, ",
                                "ELEVATION_ACCURACY, LOCAL_RELIEF, COMMENTS, PERMAFROST ",
                                "FROM PERMAFROST.PFT_LOCATIONS ",
                                "WHERE NAME = '", loc, "' "
                                ,"AND PUBLIC_FLAG = 'Y'"
                                ))
  
  tab <- reshape(tab, times=c("ID", "Name", "Latitude", "Longitude", "Elevation (m)", "Location accuracy (m)",
                              "Elevation accuracy (m)", "Local relief (m)", "Comments", "Permafrost"),
                 ids=NULL,
                 varying = c("ID", "NAME", "LATITUDE", "LONGITUDE", "ELEVATION", "LOCATION_ACCURACY",
                             "ELEVATION_ACCURACY", "LOCAL_RELIEF", "COMMENTS", "PERMAFROST"),
                 v.names= "value",
                 direction="long", sep="")
  tab <- tab[!is.na(tab$value), ]
  return(tab)
}

# Who
who.met <- function(who) {
  tab <- dbGetQuery(con, paste0("SELECT * FROM PERMAFROST.PFT_WHO ",
                                "WHERE ID IN ",
                                "('", who, "')"))
  
  tab <- reshape(tab, times=c("ID", "Name", "Institution"),
                 ids=NULL,
                 varying = c("ID", "NAME", "INSTITUTION"),
                 v.names= "value",
                 direction="long", sep="")
  tab <- tab[!is.na(tab$value), ]
  return(tab)
}

# Method
method.met <- function(method) {
  
  tab <- dbGetQuery(con, paste0("SELECT PERMAFROST.METHOD_LOOKUP.METHOD, PERMAFROST.PFT_METHOD.PRECISION, ",
                                "PERMAFROST.PFT_METHOD.ACCURACY, PERMAFROST.PFT_METHOD.SENSOR_MANUFACTURER, ",
                                "PERMAFROST.PFT_METHOD.SENSOR_MANUFACTURER_MODEL, PERMAFROST.PFT_METHOD.LOGGER_MANUFACTURER, ",
                                "PERMAFROST.PFT_METHOD.LOGGER_MANUFACTURER_MODEL, PERMAFROST.PFT_METHOD.RADIATION_SHIELD, ",
                                "PERMAFROST.PFT_METHOD.UNITS ",
                                "FROM PERMAFROST.PFT_METHOD ",
                                "INNER JOIN PERMAFROST.METHOD_LOOKUP ",
                                "ON PERMAFROST.PFT_METHOD.METHOD_ID=PERMAFROST.METHOD_LOOKUP.METHOD_ID ",
                                "WHERE PERMAFROST.PFT_METHOD.ID IN ",
                                "(", toString(method[,1]), ")"))
  
  # Set column names
  colnames(tab) <- c("Method", "Precision", "Accuracy", "Sensor manufacturer",
                     "Sensor manufacturer model", "Logger manufacturer",
                     "Logger manufacturer model", "Radiation shield",
                     "Units")
   if (nrow(method) >= 2) {
     Years <- paste0(method[,2], " - ", method[,3])
     tab <- cbind(Years, tab)
   }
  # Transpose dataframe
  tab <- t(tab)

  return(tab)
}

# Disturbance
disturbance.met <- function(loc) {
  tab <- dbGetQuery(con, paste0("SELECT TYPE, PROXIMITY, ESTIMATED_DATE, PERMAFROST.PFT_DISTURBANCE.COMMENTS ",
                       "FROM PERMAFROST.PFT_DISTURBANCE ",
                       "INNER JOIN PERMAFROST.PFT_LOCATIONS ",
                       "ON PERMAFROST.PFT_DISTURBANCE.LOCATION_ID = PERMAFROST.PFT_LOCATIONS.ID ",
                       "INNER JOIN PERMAFROST.TYPE_LOOKUP ",
                       "ON PERMAFROST.PFT_DISTURBANCE.TYPE_ID = PERMAFROST.TYPE_LOOKUP.TYPE_ID ",
                       "WHERE NAME = '", loc, "'"))
  colnames(tab) <- c("Type", "Proximity (m)", "Estimated date", "Comments")
  
    if (length(tab[,1])==0) {
      tab[1,] <- c(NA, NA, NA, NA) #c("none", "none", "none", "none")
    }
  
  tab <- t(tab)
  
  return(tab)
}

#-------------------------------------------------------------------------------------
#################### START UP #######################################################

# Load packages
library(shiny)
library(dygraphs)
library(leaflet)
library(htmlTable)
library(DT)
library(dplyr)
library(DBI)
library(leafpop)
library(plotKML)
library(rgdal)
library(shinycssloaders)
library(stringr)

library(webshot)
#webshot::install_phantomjs()


enableBookmarking("url")

# Get locations
locs <- dbGetQuery(con, paste0("SELECT NAME,",
                               " START_DATE, END_DATE,",
                               " MIN_DEPTH, MAX_DEPTH,",
                               " LATITUDE, LONGITUDE, PERMAFROST",#,
                               #" WHO_ID, METHOD_ID", #only exists in dev view, added to try and make method and who faster
                               " FROM PERMAFROST.PFT_SUMMARY_LOC",
                               " WHERE PUBLIC_FLAG = 'Y'",
                               " ORDER BY NAME ASC"
))

names(locs) <- c("name", "start_year", "end_year", "min_depth", "max_depth",
                 "lat", "long", "Permafrost")#, "who_id", "method_id")
# to make method and who faster
# locs <- locs[,!(names(locs) %in% c("who_id", "method_id"))]
# locs <- unique(locs)
# locs2 <- locs

#-------------------------------------------------------------------------------------
#################### UI ###############################################################

# Define UI for application
ui <- function(request){fluidPage( 
  
  # Set colour of Navigation bar
  tags$style(HTML("
        .navbar { background-color: #F2A900;}
        .navbar-default .navbar-nav > li > a {color:white;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #d99700;}
        .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#d99700;}
        #mymap {height: calc(100vh - 250px) !important; 
                  ")),
  
  # Setup navigation bar (Map, Temperature)
  navbarPage(title = "", id = "Navbar", 
             
             tabPanel("Map", 
                      textInput("string", "Location filter/search:", 
                                placeholder = "Search any string to filter locations",
                                value = ""),
                      leafletOutput("mymap") %>% withSpinner(color="#0097A9"),
                      br(),
                      downloadButton("downloadLoc.csv", "Download locations CSV"),
                      downloadButton('downloadLoc.kml', "Download locations KML"),
                      downloadButton('downloadLoc.shp', "Download locations shapefile")),
             
             tabPanel("Data", 
                      # Locations and years panels
                      fluidRow(
                        column(4, 
                               selectInput("loc", label = "Location:",
                                           choices=locs$name,
                                           selected="")
                        ),
                        
                        column(2,
                               selectInput("aggr", "Aggregation:",
                                           choices=c("none", "day", "month", "year"),
                                           selected="day")),
                        column(3,
                               uiOutput("secondSelection")),
                        column(3,
                               uiOutput('thirdSelection'))
                      ),
                      
                      # Setup tabs within 'Temperature'
                      tabsetPanel(id = "temp_tabs", type = 'tabs',
                                  
                                  tabPanel("Time series", 
                                           #br(),
                                           #downloadButton("downloadPlot", "Download plot"),
                                           br(),
                                           fluidRow(
                                             column(10, dygraphOutput("dygraph", width = "95%") %>% 
                                                      withSpinner(color="#0097A9")),
                                             column(2, textOutput("legend"))),
                                           br(),
                                           textOutput("dygraph_txt"),
                                           tags$head(tags$style("#dygraph_txt{color:grey;
                                                                                 font-size: 12px}"))
                                  ),
                                  tabPanel("Table", 
                                           downloadButton("downloadData", "Download"),
                                           DT::dataTableOutput("table", 
                                                               width = "95%",
                                                               height = "100%") %>% 
                                             withSpinner(color="#0097A9")),
                                  tabPanel("Ground temperature envelope", 
                                           br(),
                                           downloadButton("downloadGroundtempenv", "Download plot"),
                                           plotOutput("GroundTempEnvs",
                                                      width = "850px",
                                                      height = "600px") %>% 
                                             withSpinner(color="#0097A9"),
                                           br(),
                                           textOutput("groundTempEnv,_txt"),
                                           tags$head(tags$style("#groundTempEnv_txt{color:grey;
                                                                                         font-size: 12px}"))
                                  ),       
                                  
                                  tabPanel("Map", leafletOutput("locmap", height='500') %>% 
                                             withSpinner(color="#0097A9")),
                                  tabPanel("Metadata", 
                                           fluidRow(
                                             column(3, tableOutput("location_met")),
                                             column(3, tableOutput("who_met") %>% 
                                                      withSpinner(color="#0097A9")),
                                             column(3, tableOutput("method_met")),
                                             column(3, tableOutput("disturbance_met"))
                                           )
                                  )
                      )
             )
  )
)
}

#-------------------------------------------------------------------------------------
#################### Server #########################################################

# Define server logic
server <- shinyServer(function(input, output, session) {
  
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
                choices=  locs$name,  #"YGS_TakhiniValley",
                selected=NULL)
  })
  
  # Years selection
  output$secondSelection <- renderUI({
    # Get current location obs table
    LocObs <- currentLoc()
    # Set slider input
    sliderInput("years",
                "Years:",
                min = LocObs$start_year,
                max = LocObs$end_year,
                value = c(LocObs$start_year, LocObs$end_year),
                sep = "",
                step = 1)
  })
  
  # Depths selection
  output$thirdSelection <- renderUI({
    # Get current location obs table
    LocObs <- currentLoc()
    # Set slider input
    sliderInput("depths",
                "Depths (m):",
                min = unlist(LocObs["max_depth"]),
                max = unlist(LocObs["min_depth"]),
                value = c(unlist(LocObs["max_depth"]), unlist(LocObs["min_depth"])),
                step = 0.5)
  })
  
  ###=============================================================================
  ### Output
  ###=============================================================================
  
  ### REACTIVE OUTPUT FUNCTIONS
  # Create locations table reactive to string input
  filteredLoc <- reactive({
    validate(
      need(dim(locs[grep(input$string, locs$name, ignore.case=TRUE),])[1] !=0,
           "Search does not match any records")
    )
    filter.locs(input$string)
    })
  
  # Create current location table reactive to loc input
  currentLoc <- reactive({current.loc(input$loc)})
  
  
  # Query observations table with current loc input
  mainObs <- reactive({
    # check if currentLoc has obs data, if doesn't, it stops
    req(currentLoc()$start_year) 
    pft.query(con=con, location = input$loc)})
  
  # Aggregate and subset observations table with current aggr, depths and years input
  currentObs <- reactive({pft.subset(obs=mainObs(), aggr=input$aggr, 
                                     depth_max = input$depths[1],
                                     depth_min = input$depths[2], date_s = paste0(input$years[1], "-01-01"),
                                     date_e = paste0(input$years[2], "-12-31"))})
  
  # Plot dygraph with currentObs
  dyplot <- reactive({pft.plot(currentObs())})
  
  # Re-format currentObs for downloadData and table
  reformatTable <- reactive({pft.tformat(currentObs())})
  
  # Map so it can be used for the single location map
  currentMap <- reactive({pft.map(filteredLoc())})
  
  
  ### OUTPUTS
  
  # All locations MAP
  output$mymap <- renderLeaflet({
    currentMap()
  })
  
  # Single location MAP
  output$locmap <- renderLeaflet({
    cloc <- currentLoc()
    setView(currentMap(), lng=cloc$long, lat=cloc$lat, zoom=15)
  })
  
  ### Locations download
  # CSV download
  output$downloadLoc.csv <- downloadHandler(
    filename = paste0("TempLocations_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(locs, file)
    }
  )
  # KML download
  output$downloadLoc.kml <- downloadHandler(
    filename = paste0("TempLocations_", Sys.Date(), ".kml"),
    content = function(file) {
      xy <- locs[, c("long", "lat")]
      shp <- SpatialPointsDataFrame(xy, locs, proj4string = CRS("+proj=longlat +datum=NAD83"))
      # aesthetics
      pal <- colorFactor(c("#DC4405", "grey40", "#0097A9"), domain = c("no", "undetermined", "yes"))
      plotKML(shp, folder.name = "Temperature locations", file, 
              points.names = shp$name)
    }
  )
  # Shapefile download
  output$downloadLoc.shp <- downloadHandler(
    filename = function() {
      paste0("TempLocations_", Sys.Date(), ".zip")
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
      writeOGR(shp, tempdir, 'TempLocations', "ESRI Shapefile", overwrite_layer=TRUE)
      # zip shapefile files
      zip_file <- file.path(tempdir, "TempLocations.zip") # construct path to file
      shp_files <- list.files(tempdir, "TempLocations", full.names = FALSE) # character list of files in temp_shp
      zip(zipfile= file, files = shp_files)
    }
  )
  
  ### Tab show/hide reactive expression
  tab_hide <- reactive({
    if (is.na(currentLoc()$start_year)){
      tab <- "hide"
    } else if (!is.na(currentLoc()$start_year)) {
      tab <- "show"}
  })
  
  observe({
    if (tab_hide()=="show") {
      showTab("temp_tabs", "Time series")
      showTab("temp_tabs", "Table")
      showTab("temp_tabs", "Ground temperature envelope")
    } else if (tab_hide()=="hide") {
      hideTab("temp_tabs", "Time series")
      hideTab("temp_tabs", "Table")
      hideTab("temp_tabs", "Ground temperature envelope")
    }
  })
  
  ### Time series
  output$dygraph <- renderDygraph({
    dyplot()
  })
  
  # # Time series download
  # output$downloadPlot <- downloadHandler(
  #   filename = function() {paste0("TimeSeries_", currentLoc()$name, ".png")},
  #   content = function(file) {
  #     saveWidget(dyplot(), "temp.html", selfcontained = FALSE)
  #     webshot("temp.html", file = file)
  #   },
  #   contentType = 'image/png'
  # )
  
  # Time series description
  output$dygraph_txt <- renderText({
    paste0("This graph shows temperature evolution (y-axis) over time (x-axis), ",
           "where every line represents a different depth.")
  })
  
  ### Table
  output$table <- renderDataTable({
    datatable(reformatTable(), rownames=FALSE, options = list(pageLength = 15))
  })
  # Data download
  output$downloadData <- downloadHandler(
    filename = paste0(currentLoc(), "-", Sys.Date(), ".csv"),
    content = function(file) {
      obs <- reformatTable()
      colnames(obs) <- gsub(pattern=" ", replacement="_", x=colnames(obs))
      write.csv(obs, file, row.names=FALSE)
    })
  
  ### Ground temperature envelopes
  output$GroundTempEnvs <- renderPlot({
    pft.plot_groundtempenv(mainObs(), paste0(input$years[1], "-01-01"),
                           paste0(input$years[2], "-12-31"))
  })
  # Ground temperature envelopes description
  output$groundTempEnvs_txt <- renderText({
    paste0("A ground temperature envelope displays annual permafrost thermal regimes. ",
           "The red, blue and black lines represent maximum, minimum and mean ",
           "annual temperatures, respectively")
  })
  # Ground temperature envelope download
  output$downloadGroundtempenv <- downloadHandler(
    filename = function() {paste0("GroundTempEnvelope_", currentLoc()$name, ".png")},
    content = function(file) {
      ggsave(file, plot = pft.plot_groundtempenv(mainObs(), paste0(input$years[1], "-01-01"),
                                                 paste0(input$years[2], "-12-31")),
             device = "png")
    }
  )
  
  
  ### Metadata
  # Location metadata
  output$location_met <- renderTable({
    location.met(input$loc)
  },
  colnames = FALSE, caption = "Location",
  caption.placement = getOption("xtable.caption.placement", "top"),
  )
  
  # Who metadata
  output$who_met <- renderTable({
    who_id <- dbGetQuery(con, paste0("SELECT DISTINCT WHO_ID ",
                                     "FROM PERMAFROST.PFT_SUMMARY ",
                                     "WHERE NAME = '", input$loc, "'"))
    who_id <- who_id[[1]]
    #who_id <- unique(mainObs()$who_id)
    who.met(who_id)
  },
  colnames = FALSE, caption = "Who",
  caption.placement = getOption("xtable.caption.placement", "top"),
  )
  
  # Method metadata
  output$method_met <- renderTable({
    method_id <- dbGetQuery(con, paste0("SELECT DISTINCT(METHOD_ID), MIN(YEAR) AS START_YEAR, MAX(YEAR) AS END_YEAR ",
                                        "FROM PERMAFROST.PFT_SUMMARY ",
                                        "WHERE NAME = '", input$loc, "'",
                                        "AND DEPTH <= 0 ",
                                        "GROUP BY METHOD_ID ",
                                        "ORDER BY START_YEAR"))

    method.met(method_id)
  },
  colnames = FALSE, rownames = TRUE, caption = "Method", 
  caption.placement = getOption("xtable.caption.placement", "top"),
  )
  
  # Disturbance metadata
  output$disturbance_met <- renderTable({
    
    disturbance.met(input$loc)
  },
  colnames = FALSE, rownames = TRUE, caption = "Disturbance",
  caption.placement = getOption("xtable.caption.placement", "top"),
  )
  
  })

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")


#-------------------------------------------------------------------------------------