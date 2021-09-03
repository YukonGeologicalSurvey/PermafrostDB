  )
)

server <- function(input, output) {
  
  # sorted columns are colored now because CSS are attached to them
  output$PermafrostTab <- DT::renderDataTable({
    DT::datatable(pfReports, options = list(orderClasses = TRUE))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$GeotechTab <- DT::renderDataTable({
    DT::datatable(gtReports, options = list(orderClasses = TRUE))
  })
  
}

shinyApp(ui, server)