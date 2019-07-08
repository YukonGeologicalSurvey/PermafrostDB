#################################################################################
########### Trumpet curve and time series plot combinatino app ##################
#################################################################################
#    http://shiny.rstudio.com/
# Requires that a connection (con) has already been made to the database


library(shiny)
# Get locations
locs <- dbGetQuery(con, paste0("SELECT UNIQUE NAME, EXTRACT(year FROM START_DATE),", 
                               " EXTRACT(year FROM END_DATE),",
                               " MIN_DEPTH, MAX_DEPTH",
                               " FROM PFT_SUMMARY_LOC",
                               " ORDER BY NAME")) 
names(locs) <- c("name", "start_year", "end_year", "min_depth", "max_depth")

#################### Reactive table and dygraph query ##########################
pft.query <- function(con, aggr, location, depth_min, depth_max, date_s, date_e) {
    
    if (aggr=="none") {
        obs <- dbGetQuery(con, paste0("SELECT TEMPTIME, DEPTH, ROUND(TEMP, 2) AS TEMP ",
                                      "FROM PFT_SUMMARY ",
                                      "WHERE NAME = '", location, "' ",
                                      "AND DEPTH >= ", depth_max, " AND DEPTH <=", depth_min, 
                                      " AND TEMPTIME BETWEEN '", date_s ,"' AND '", date_e, "'",
                                      "ORDER BY TEMPTIME, DEPTH DESC"))
        obs$TEMPTIME <- as.POSIXct(obs$TEMPTIME, tz = "", format = "%Y-%m-%d %H:%M")
        
    } else if (aggr=="day") {
        obs <- dbGetQuery(con, paste0("SELECT TEMPDATE, DEPTH, ROUND(AVG(TEMP), 2) AS DAILYTEMP ",
                                      "FROM PFT_SUMMARY ",
                                      "WHERE NAME = '", location, "' ",
                                      "AND DEPTH >= ", depth_max, " AND DEPTH <=", depth_min, 
                                      " AND TEMPDATE BETWEEN '", date_s ,"' AND '", date_e, "'",
                                      "GROUP BY TEMPDATE, DEPTH ",
                                      "ORDER BY TEMPDATE, DEPTH DESC")) 
        obs$TEMPDATE <- as.Date(obs$TEMPDATE)
        
    } else if (aggr=="month") {
        obs <- dbGetQuery(con, paste0("SELECT MONTH, DEPTH, ROUND(AVG(TEMP), 2) AS DAILYTEMP ",
                                      "FROM PFT_SUMMARY ",
                                      "WHERE NAME = '", location, "' ",
                                      "AND DEPTH >= ", depth_max, " AND DEPTH <=", depth_min, 
                                      " AND TEMPDATE BETWEEN '", date_s ,"' AND '", date_e, "'",
                                      "GROUP BY MONTH, DEPTH ",
                                      "ORDER BY MONTH, DEPTH DESC")) 
        obs$MONTH <- as.Date(obs$MONTH)
        
    } else if (aggr=="year") {
        obs <- dbGetQuery(con, paste0("SELECT YEAR, DEPTH, ROUND(AVG(TEMP), 2) AS DAILYTEMP ",
                                      "FROM PFT_SUMMARY ",
                                      "WHERE NAME = '", location, "' ",
                                      "AND DEPTH >= ", depth_max, " AND DEPTH <=", depth_min, 
                                      " AND TEMPDATE BETWEEN '", date_s ,"' AND '", date_e, "'",
                                      "GROUP BY YEAR, DEPTH ",
                                      "ORDER BY YEAR, DEPTH DESC")) 
        obs$YEAR <- as.Date(obs$YEAR)
    }
    
    names(obs) <- c("date", "depth", "temp")
    return(obs)

}

#################### Time series plotting function #############################
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
    graph <- dygraph(qxts, 
                     #main=paste(location, collapse=", "),
                     ylab = "Temperature")
    
    # iterate to create series labels
    for (seriesNum in 1:snamesLen)
    {
        graph <- graph %>% dySeries(paste0("V",seriesNum), label = snames[seriesNum])
    }
    
    graph <- graph %>%
        dyLegend(width = 400) %>%
        dyOptions(labelsUTC = TRUE) %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
    
    # display graph
    graph 
    
}

#################### Table formatting function #################################
pft.tformat <- function(table) {
    table <- reshape(table, idvar = 'date', v.names = 'temp', timevar = 'depth', direction = "wide")
    table$date <- as.character(table$date)
    return(table)
}

#################### Trumpet curve plotting function #############################
pft.plot_trumpetcurve <- function(con, location, depth_max, depth_min, year) {
    
    obs <- NULL
    
        # Query from db
        obs <- dbGetQuery(con, paste0("SELECT DEPTH, YEAR, ROUND(AVG(TEMP), 2) AS MEAN,",
                                       "ROUND(MAX(TEMP), 2) AS MAX, ROUND(MIN(TEMP), 2) AS MIN",
                                      ", COUNT(DISTINCT TEMPDATE) AS COUNTS ",
                                       " FROM PFT_SUMMARY ",
                                       "WHERE NAME = '", location, "' ",
                                       " AND YEAR IN (", paste(as.numeric(year), sep="", collapse=", "), ") ",
                                       "AND DEPTH >= ", depth_max, " AND DEPTH <=", depth_min, 
                                       "GROUP BY DEPTH, YEAR ",
                                       "ORDER BY DEPTH DESC"))
        
        # set col names
        names(obs) <- c("depth", "year", "mean", "max", "min", "count")
     
    years <- NULL  
    annot <- NULL
       
    for (i in unique(obs$year)) {
        min <- min(obs[obs$year==i,]$count)
        
        if (min < 350) {
            anno <- paste("Inc dataset")
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
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        coord_flip() +
        labs(title=paste0(location), x="depth (m)", y="temperature") +
        geom_hline(yintercept = 0, linetype="dashed", color="grey") +
        # geom_smooth(aes(x=depth, y=max), method='loess', se=FALSE, span=0.35, color="firebrick2", size=0.75) +
        # geom_smooth(aes(x=depth, y=min), method='loess', se=FALSE, span=0.35, color="steelblue3", size=0.75) +
        # geom_smooth(aes(x=depth, y=mean), method='loess', se=FALSE, span=0.35, color="grey20", size=0.75) + 
        geom_line(aes(x=depth, y=max), color="firebrick2", size=0.75) +
        geom_line(aes(x=depth, y=min), color="steelblue3", size=0.75) +
        geom_line(aes(x=depth, y=mean), color="grey20", size=0.75) + 
        geom_text(data=annots, aes(x=min(obs$depth) + 1, y=mean(obs$min), label=annot, col="red")) +
        facet_wrap(~year) 
    
    print(plt)
    
}


#################### Reactive function for obs subset ##########################
current.locobs <- function(n){
    cobs <- locs[locs$name==n,]
    return(cobs)
}
#################### UI ########################################################
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("SUPER COOL RSHINY APP!!! :O"),
    
    # Locations and years panels
    fluidRow(
        column(3,
               selectInput("loc", "Location:",
                           choices=locs$name)),
        column(3,
               selectInput("aggr", "Aggregation:",
                           choices=c("none", "day", "month", "year"),
                           selected="day")),
        column(3,
               uiOutput("secondSelection")),
        column(3,
               uiOutput('thirdSelection'))
    ),
    
    # Plot tabs
            tabsetPanel(type = 'tabs',
                tabPanel("Time series", dygraphOutput("dygraph")),
                tabPanel("Table", 
                         downloadButton("downloadData", "Download"),
                         tableOutput("table")),
                tabPanel("Trumpet curves", plotOutput("TrumpetCurves"))
            )
    )


#################### Server ####################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ###
    ### Selection
    ###
    currentLocObs <- reactive({current.locobs(input$loc)})
    
    output$secondSelection <- renderUI({
        sliderInput("years",
                    "Years:",
                    min = currentLocObs()["start_year"],
                    max = currentLocObs()["end_year"],
                    value = c(2018, 2019))
    })
    
    output$thirdSelection <- renderUI({
        sliderInput("depths",
                    "Depths:",
                    min = unlist(currentLocObs()["max_depth"]),
                    max = unlist(currentLocObs()["min_depth"]),
                    value = c(unlist(currentLocObs()["max_depth"]), unlist(currentLocObs()["min_depth"])))
    })
    
    ###
    ### Output
    ###
    currentObs <- reactive({pft.query(con, aggr=input$aggr, location = input$loc, depth_max = input$depths[1],
                                      depth_min = input$depths[2], date_s = paste0(input$years[1], "-01-01"),
                                      date_e = paste0(input$years[2], "-01-01"))})
    # Time series
    output$dygraph <- renderDygraph({
        obs <- currentObs()
        pft.plot(obs)
    })
    
    # Data download
    output$downloadData <- downloadHandler(
        filename = paste0(input$loc, "-", Sys.Date(), ".csv"),
        content = function(file) {
            obs <- currentObs()
            obs <- pft.tformat(obs)
            colnames(obs) <- gsub(pattern="temp.", replacement="", x=colnames(obs))
            write.csv(obs, file, row.names=FALSE)
        }
    )
    
    # Table
    output$table <- renderTable({
        # Get reactive table
        obs <- currentObs()
        # Format
        obs <- pft.tformat(obs)
        colnames(obs) <- gsub(pattern="temp.", replacement="", x=colnames(obs))
        obs
    })
    
    # Trumpet curves
    output$TrumpetCurves <- renderPlot({
        # Plot trumpet curves
        location <- input$loc
        depth_max <- -20
        depth_min <- 0
        year <- seq(from=input$years[1], to=input$years[2], by=1)
        
        pft.plot_trumpetcurve(con, location, depth_max, depth_min, year)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

