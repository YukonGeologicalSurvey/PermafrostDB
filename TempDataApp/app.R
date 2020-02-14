#######################################################################################
#--------------# Trumpet curve and time series plot combinatino app #-----------------#
#######################################################################################

pft.dbconnect <- function(classPath, username, password) {
    require(RJDBC)
    require(DBI)
    
    jdbcDriver <- JDBC("oracle.jdbc.OracleDriver", classPath)
    
    con <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//199.247.170.218:1521/YGS50D", username, password)
    
    return(con)
}

# Connect to database
con <- pft.dbconnect(classPath= "C:/ojdbc6.jar",
                     username="", password="")

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


enableBookmarking("url")

# Get locations
locs <- dbGetQuery(con, paste0("SELECT NAME, EXTRACT(year FROM START_DATE),",
                               " EXTRACT(year FROM END_DATE),",
                               " MIN_DEPTH, MAX_DEPTH,",
                               " LATITUDE, LONGITUDE, PERMAFROST",
                               " FROM YGSIDS.PFT_SUMMARY_LOC",
                               " ORDER BY NAME DESC"
                               ))

names(locs) <- c("name", "start_year", "end_year", "min_depth", "max_depth",
                 "lat", "long", "permafrost")

#-------------------------------------------------------------------------------------
#################### FUNCTIONS #######################################################
########## f.link: Add link to locs ##########
f.link <- function(locs) {
    links <- c()
    for (i in locs$name) {
        link <- c(paste0("'<a href = \"/?_inputs_&aggr=%22day%22&aggr-selectized=%22%22",
                         "&loc=%22", i, "%22&loc-selectized=%22%22",
                         "&Navbar=%22Temperature%22\"> See site data here </a>'"))
        links <- c(links, link)
    }
    locsl <- cbind(locs, links)
  
    return(locsl)
}
########## pft.map: Leaflet map ##########
pft.map <- function(loc, zoom=FALSE) {
    
    # Set up icon colours
    pal <- colorFactor(c("#DC4405", "grey40", "#0097A9"), domain = c("no", "undetermined", "yes"))
    # Create map
     if (zoom==TRUE){
         loc <- as.data.frame(loc)
         leaflet(loc) %>%
             addProviderTiles('Esri.WorldTopoMap') %>% # More here: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
             addCircleMarkers(lng=loc$long, lat=loc$lat, 
                              popup=leafpop::popupTable(loc, row.numbers=FALSE, feature.id=FALSE),
                              color = ~pal(permafrost), opacity=1)  %>%
             leaflet::addLegend("topright", pal = pal, values = ~permafrost)
     }
    else if (zoom==FALSE){
        leaflet(loc) %>%
            addProviderTiles('Esri.WorldTopoMap') %>% # More here: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
            addCircleMarkers(lng=loc$long, lat=loc$lat,
                             popup=leafpop::popupTable(f.link(loc), row.numbers=FALSE, feature.id=FALSE),
                             color = ~pal(permafrost), opacity=1) %>%
            leaflet::addLegend("topright", pal = pal, values = ~permafrost)
    }
    
}


########## f.tabhs: Create location html tables ###########
f.tabhs <- function(locs) {
    tabhs <- NULL
    for (i in locs$name) {
        x <- locs[locs$name==i,]
        x <- reshape(x, times=c("Start year", "End year", "Min depth", "Max depth", "Latitude", "longitude", "Permafrost"),
                     ids=NULL,
                     varying = c("start_year", "end_year", "min_depth", "max_depth", "lat", "long", "permafrost"),
                     v.names= "value",
                     direction="long", sep="")
        rownames(x) <- x$time
        x <- x[, c("time", "value")]
        
        tabh <- htmlTable(x, rnames = FALSE, caption = i, header=c("", ""), align=paste(rep('l,c',ncol(x)),collapse=''))
        tabhs <- c(tabhs, tabh)
    }
    return(tabhs)
}
########## pft.query and pft.subset: Reactive table and dygraph query and subset #################################

## Query daily average data from PFT_SUMMARY in database 
pft.query <- function(con, location) { #aggr
    # if (aggr=="none") {
    #     obs <- dbGetQuery(con, paste0("SELECT TEMPTIME, DEPTH, ROUND(TEMP, 2) AS TEMP ",
    #                                   "FROM PFT_SUMMARY ",
    #                                   "WHERE NAME = '", location, "' ",
    #                                   "ORDER BY TEMPTIME, DEPTH DESC"))
    #     obs$TEMPTIME <- as.POSIXct(obs$TEMPTIME, tz = "", format = "%Y-%m-%d %H:%M")
    #     
    # } else {
        obs <- dbGetQuery(con, paste0("SELECT TEMPDATE, MONTH, YEAR, DEPTH, ROUND(AVG(TEMP), 2) AS DAILYTEMP, ",
                                      "WHO_ID, METHOD_ID ",
                                      "FROM YGSIDS.PFT_SUMMARY ",
                                      "WHERE NAME = '", location, "' ",
                                      "GROUP BY TEMPDATE, NAME, DEPTH, MONTH, YEAR, WHO_ID, METHOD_ID ",
                                      "ORDER BY TEMPDATE, DEPTH DESC"))
        obs$TEMPDATE <- as.Date(obs$TEMPDATE)
        obs$MONTH <- as.Date(obs$MONTH)
        obs$YEAR <- as.Date(obs$YEAR)
    # } 
    names(obs) <- c("date", "month", "year", "depth", "temp", "who_id", "method_id")
    return(obs)
}

## Aggregate and subset data from pft.query
pft.subset <- function(obs, aggr, depth_min, depth_max, date_s, date_e) {
    
    if (aggr=="none") {
        obs <- dbGetQuery(con, paste0("SELECT TEMPTIME, DEPTH, ROUND(TEMP, 2) AS TEMP ",
                                      "FROM YGSIDS.PFT_SUMMARY ",
                                      "WHERE NAME = '", location, "' ",
                                      "AND DEPTH >= ", depth_max, " AND DEPTH <=", depth_min, 
                                      " AND TEMPTIME BETWEEN '", date_s ,"' AND '", date_e, "'",
                                      "ORDER BY TEMPTIME, DEPTH DESC"))
        obs$TEMPTIME <- as.POSIXct(obs$TEMPTIME, tz = "", format = "%Y-%m-%d %H:%M")
        
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

## Plot the data from obs in a dygraph time eries graph
pft.plot <- function(obs) {
    
    require(dygraphs)
    require(xts)
    
    date <- data.frame(date=unique(obs$date))
    
    depths <- unique(obs$depth)
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
    graph <- dygraph(qxts, ylab = "Temperature")
    
    # iterate to create series labels
    for (seriesNum in 1:snamesLen)
    {
        graph <- graph %>% dySeries(paste0("V",seriesNum), label = snames[seriesNum])
    }
    
    # Create plot
    graph <- graph %>%
        dyLegend(width = 400) %>%
        dyOptions(labelsUTC = TRUE, gridLineWidth = 0.1) %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
    
    # display graph
    graph 
    
}

########## pft.format: Table formatting function ########################################

## Formatting obs table from long to wide
pft.tformat <- function(table) {
    table <- reshape(table, idvar = 'date', v.names = 'temp', timevar = 'depth', direction = "wide")
    table$date <- as.character(table$date)
    colnames(table) <- gsub(pattern="temp.", replacement="", x=colnames(table))
    return(table)
}

########## pft.plot_trumpetcurve: Trumpet curve plotting function ##################################

## Plot trumpet curve for all years between date_s and date_e 
pft.plot_trumpetcurve <- function(obs, date_s, date_e) { 
    
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
        mx <- max(obs[obs$year==i,]$count)
        
        if (mn < 350) {
            anno <- NULL
            anno <- paste0("Incomplete dataset:\nmin: ", mn, "\n", "max: ", mx)
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
        labs(x="depth (m)", y="temperature") +
        geom_hline(yintercept = 0, linetype="dashed", color="grey") +    # vertical line at 0
        geom_vline(xintercept = 0, color="grey") +    # Horizontal line at 0
        
        geom_line(aes(x=depth, y=max), color="#DC4405", size=0.75) +     # Maximum line
        geom_point(aes(x=depth, y=max), color="#DC4405", size=2) + # firebrick2
        
        geom_line(aes(x=depth, y=min), color="#0097A9", size=0.75) +     # Minimum line
        geom_point(aes(x=depth, y=min), color="#0097A9", size=2) + # steelblue3
        
        geom_line(aes(x=depth, y=mean), color="grey20", size=0.75) +     # Mean line
        geom_point(aes(x=depth, y=mean), color="grey20", size=2) + 
        
        geom_text(data=annots, aes(
            x=max(obs$depth) + .75*min(obs$depth), y=mean(obs$max) + 2,
            hjust=0.4, vjust=0.7,# Add incomplete dataset text
            label=annot, col="#F2A900"),
            show.legend=FALSE) +
        facet_wrap(~year) 
    
    print(plt)
    
}


########## current.loc: Reactive function for locs subset ###########

## Subsets the locs table to a single location
current.loc <- function(n){
    cloc <- locs[locs$name==n,]
    return(cloc)
}

########## location.met, who.met, method.met: Meta data tables functions ###############
## Metadata
# Location
location.met <- function(loc) {
    tab <- dbGetQuery(con, paste0("SELECT * FROM YGSIDS.PFT_LOCATIONS ",
                                  "WHERE NAME = '", loc, "'"))
    
    tab <- reshape(tab, times=c("ID", "Name", "Latitude", "longitude", "Elevation", "Location accuracy",
                                "Elevation accuracy", "Local relief", "Comments", "Permafrost"),
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
    tab <- dbGetQuery(con, paste0("SELECT * FROM YGSIDS.PFT_WHO ",
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
    tab <- dbGetQuery(con, paste0("SELECT * FROM YGSIDS.PFT_METHOD ",
                                  "WHERE ID IN ",
                                  "('", method, "')"))
    
    tab <- reshape(tab, times=c("ID", "Precision", "Accuracy", "Method", "Sensor manufacturer",
                                "Sensor manufacturer model", "Sensor manufacturer date", "Logger manufacturer",
                                "Logger manufacturer model", "logger manufacturer date", "Radiation shield",
                                "units"),
                   ids=NULL,
                   varying = c("ID", "PRECISION", "ACCURACY", "METHOD", "SENSOR_MANUFACTURER",
                               "SENSOR_MANUFACTURER_MODEL", "SENSOR_MANUFACTURER_DATE", "LOGGER_MANUFACTURER",
                               "LOGGER_MANUFACTURER_MODEL", "LOGGER_MANUFACTURER_DATE", "RADIATION_SHIELD",
                               "UNITS"),
                   v.names= "value",
                   direction="long", sep="")
    tab <- tab[!is.na(tab$value), ]
    return(tab)
}


#-------------------------------------------------------------------------------------
#################### UI ###############################################################

# Define UI for application
ui <- function(request){fluidPage( 
    
    # Application title
    titlePanel(title = span("Yukon Ground Temperature Data", 
                            style = "color: Black; font-size: 28px")
                ),
    
    # Set colour of Navigation bar
    tags$style(HTML(" 
        .navbar { background-color: #F2A900;}
        .navbar-default .navbar-nav > li > a {color:white;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #d99700;}
        .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#d99700;}
                  ")),
    
    # Setup navigation bar (Map, Temperature)
    navbarPage(title = "", id = "Navbar",
                
               tabPanel("Map", 
                        leafletOutput("mymap", height='750') %>% withSpinner(color="#0097A9"),
                        downloadButton("downloadLoc.csv", "Download locations CSV"),
                        downloadButton('downloadLoc.kml', "Download locations KML"),
                        downloadButton('downloadLoc.shp', "Download locations shapefile")),
               tabPanel("Temperature", 
                        # Locations and years panels
                        fluidRow(
                            column(4,
                                   selectInput("loc", "Location:",
                                               choices=locs$name,
                                               selected="")),
                            column(2,
                                   selectInput("aggr", "Aggregation:",
                                               choices=c("day", "month", "year"), #none
                                               selected="day")),
                            column(3,
                                   uiOutput("secondSelection")),
                            column(3,
                                   uiOutput('thirdSelection'))
                        ),
                        
                        # Setup tabs within 'Temperature'
                        tabsetPanel(type = 'tabs',
                                      
                                    tabPanel("Time series", 
                                             dygraphOutput("dygraph", width = "95%") %>% 
                                               withSpinner(color="#0097A9"),
                                             br(),
                                             textOutput("dygraph_txt"),
                                              tags$head(tags$style("#dygraph_txt{color:grey;
                                                                                 font-size: 12px}")),
                                             br(),
                                             htmlOutput("locsmeta")
                                             ),
                                    tabPanel("Table", 
                                             downloadButton("downloadData", "Download"),
                                             DT::dataTableOutput("table", 
                                                                 width = "95%",
                                                                 height = "100%") %>% 
                                               withSpinner(color="#0097A9")),
                                    tabPanel("Trumpet curves", plotOutput("TrumpetCurves",
                                                                          width = "850px",
                                                                          height = "600px") %>% 
                                              withSpinner(color="#0097A9"),
                                              br(),
                                              textOutput("trumpetCurves_txt"),
                                                tags$head(tags$style("#trumpetCurves_txt{color:grey;
                                                                                         font-size: 12px}"))
                                             ),       
                                              
                                    tabPanel("Map", leafletOutput("locmap", height='500') %>% 
                                               withSpinner(color="#0097A9")),
                                    tabPanel("Metadata", 
                                             fluidRow(
                                                 column(4, tableOutput("location_met")),
                                                 column(4, tableOutput("who_met") %>% 
                                                          withSpinner(color="#0097A9")),
                                                 column(4, tableOutput("method_met"))
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
                    "Depths:",
                    min = unlist(LocObs["max_depth"]),
                    max = unlist(LocObs["min_depth"]),
                    value = c(unlist(LocObs["max_depth"]), unlist(LocObs["min_depth"])),
                    step = 0.5)
    })
    
    ###=============================================================================
    ### Output
    ###=============================================================================
    
    ### REACTIVE OUTPUT FUNCTIONS
    # Create current location table reactive to loc input
    currentLoc <- reactive({current.loc(input$loc)})
    
    
    # Query observations table with current loc input
    mainObs <- reactive({pft.query(con=con, #aggr = input$aggr, 
                                   location = input$loc)})
    
    # Aggregate and subset observations table with current aggr, depths and years input
    currentObs <- reactive({pft.subset(obs=mainObs(), aggr=input$aggr, 
                                       depth_max = input$depths[1],
                                       depth_min = input$depths[2], date_s = paste0(input$years[1], "-01-01"),
                                       date_e = paste0(input$years[2], "-12-31"))})
    
    # Create html table from current locations table
    currentLocTabh <- reactive({f.tabhs(locs = currentLoc())}) # Not currently in use
    
    # Re-format currentObs for downloadData and table
    reformatTable <- reactive({pft.tformat(currentObs())})
    
    ### OUTPUTS
    # All locations MAP
    output$mymap <- renderLeaflet({
        pft.map(locs, zoom=FALSE)
    })
    
    # Single location MAP
    output$locmap <- renderLeaflet({
        pft.map(loc=currentLoc(), zoom=TRUE)
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
        zip(zipfile= file, files = shp_files, shape=)
      }
    )
    
    output$dygraph <- renderDygraph({
      obs <- currentObs()
      pft.plot(obs)
    })
    # Time series description
    output$dygraph_txt <- renderText({
      paste0("This graph shows temperature evolution (y-axis) over time (x-axis), ",
             "where every line represents a different depth.", input$n)
    })
    
    # Summary locations table to go below time series
    output$locsmeta <- renderUI({
      HTML(
        f.tabhs(locs = currentLoc()) 
      )
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
        colnames(obs) <- gsub(pattern="temp.", replacement="", x=colnames(obs))
        write.csv(obs, file, row.names=FALSE)
      })
    
    ### Trumpet curves
    output$TrumpetCurves <- renderPlot({
      pft.plot_trumpetcurve(mainObs(), paste0(input$years[1], "-01-01"),
                            paste0(input$years[2], "-12-31"))
    })
    # Trumpet curves description
    output$trumpetCurves_txt <- renderText({
      paste0("A trumpet curve displays annual permafrost thermal regimes. ",
             "The red, blue and black lines represent maximum, minimum and mean ",
             "annual temperatures, respectively", input$n)
    })
    
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
      who_id <- unique(mainObs()$who_id)
      who.met(who_id)
    },
    colnames = FALSE, caption = "Who", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    )
    
    # Method metadata
    output$method_met <- renderTable({
      method_id <- unique(mainObs()$method_id)
      method.met(method_id)
    },
    colnames = FALSE, caption = "Method", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    )
})

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")


#-------------------------------------------------------------------------------------





