pft.dbconnect <- function(classPath, username, password) {
  require(RJDBC)
  require(DBI)
  
  jdbcDriver <- JDBC("oracle.jdbc.OracleDriver", classPath)
  
  con <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//199.247.170.218:1521/YGS50D", username, password)
  
  return(con)
}

# Connect to database
con <- pft.dbconnect(classPath= "",username="", password="")
