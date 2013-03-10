destinationArrivalDelayStatsSQL <-
function(airports, database, table){
  dr = dbDriver("SQLite")
  con = dbConnect(dr, dbname = database)
  
  state = (1:length(airports))
  for (i in 1:length(airports)){
    
    #get average arrival delay for airport
    avCMD = sprintf("SELECT AVG(ArrDelay) FROM %s WHERE dest = '%s'", table, airports[i])
    avD = dbSendQuery(con, avCMD)
    AvgArrDelay = fetch(avD)
    
    #get all arrival delays for airport
    depCMD = sprintf("SELECT ArrDelay FROM %s WHERE dest = '%s'", table, airports[i])
    depFlights = sqliteQuickSQL(con, depCMD)
    
    #number of departing flights
    numFlights = dim(depFlights)[1]
    
    squared = depFlights^2
    sumSquared = sum(squared)
    numerator = sumSquared - (numFlights * AvgArrDelay^2)
    airportSD = (numerator/(numFlights-1))^.5
    
    state[i] = sprintf("Destination Airport: %s Avg Delay: %f  SD:%f", airports[i], AvgArrDelay, airportSD)
    
    
    
  }
  state
}
