library(shiny)
library(ggplot2)  # for the diamonds dataset

source('dbconnection.R')

jscode <- "$('a').attr('target','_blank');"

pfReports <- dbGetQuery(con, "SELECT YEAR, TITLE, AUTHOR, REPORT_TYPE, DESCRIPTION from permafrost.VW_PERMAFROST_REPORTS")
gtReports <- dbGetQuery(con, "SELECT YEAR, TITLE, REFERENCE_NUM, AUTHOR, REPORT_TYPE, LOCATION from permafrost.VW_GEOTECHNICAL_REPORTS")

ui <- fluidPage(
  tags$script(jscode),
  tags$style(HTML("
        .nav{padding-left: 30px;margin-bottom: 20px;}
        .nav-tabs { background-color: #F2A900; width:130%}
        .nav-tabs > li > a {color:white;border: none;}
        .nav-tabs > .active > a,
        .nav-tabs > .active > a:focus,
        .nav-tabs > .active > a:hover {color: white;background-color: #d99700; border: 0px solid #ddd;}
        .nav-tabs > li > a:hover {color: white;background-color:#d99700;}
        .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {color: white; background-color: #d99700; border: none;border-radius: 0px 0px 0 0;}
        .tab-content {width:130%;}
                  ")),

  title = "Permafrost and Geotechnical Reports",
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Permafrost", DT::dataTableOutput("PermafrostTab")),
        tabPanel("Geotechnical", DT::dataTableOutput("GeotechTab"))
      
    )
  )
)

server <- function(input, output) {
  
  # sorted columns are colored now because CSS are attached to them
  output$PermafrostTab <- DT::renderDataTable({
    DT::datatable(pfReports, options = list(autoWidth = FALSE, orderClasses = TRUE), escape=FALSE)
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$GeotechTab <- DT::renderDataTable({
    DT::datatable(gtReports, options = list(autoWidth = TRUE, orderClasses = TRUE), escape=FALSE)
  })
  
}

shinyApp(ui, server)
