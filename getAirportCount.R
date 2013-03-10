getAirportCount <-
function(airports, database, table){
  
  dr = dbDriver("SQLite")
  con = dbConnect(dr, dbname = database)
  
  cmd = sprintf("SELECT origin, count(origin) FROM %s GROUP BY origin", table)

  dbCount = dbSendQuery(con, cmd)
    
  outCount = fetch(dbCount)  
  
  
    
    
    
}
