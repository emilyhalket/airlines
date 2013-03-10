#gets arrival delays for flights landing in airports of interest

#read in blocks using str.split (loop?) and calculate mean & sd

arrivingAirportArrDelayStatsCompressed <- function( dest = c("LAX", 'SMF', 'OAK', 'SFO'), files, B){
  #set con
  
  
  
  total <- structure(integer(length(dest)), names = dest)
  #keep track of total flights for airport
  
  means <- structure(integer(length(dest)), names = dest)
  #keep track of mean delay
  means[] <- NA  #set to NA so when I take mean later I don't have 0 involved
  squares <- structure(integer(length(dest)), names = dest)
  #keep track of sqaures ( to sum ) for airport
  
  sds <- structure(integer(length(dest)), names = dest) 
  for (i in 1:length(dest)){ #loop over airports
    
    grepCmddest <- sprintf("grep %s %s", dest[i], files)
    con = pipe(grepCmddest, "r")
    
    
    while(TRUE){
      
      
      ll <- readLines(con, n=B)  
      #because my file is open, I can just pick up where i left off in loop
      # i don't want to use[-numberISaw] because then I read it only to throw it away
      if(length(ll) ==0)
        break
      
      destSplit = sapply(strsplit(ll, ","), '[[', 4)
      # array of dests
      arrDelaySplit = sapply(strsplit(ll, ","), '[[', 1)
      #array of arrival delays
      airportdest = destSplit == dest[i]
      #find where dest equals airport of interest
      destDelays = arrDelaySplit[airportdest]
      #index delays for airports of interest
      airportArrivalDelays = as.numeric(destDelays) #if contains header --> NA value
      
      #take care of NAs?
      
      
      subcounts <- sum(airportdest)#counts what I want by dest
      #subcounts[is.na(subcounts)] = 0
      
      total[[i]] <-  total[[i]] + subcounts #update total num of flights/dest
      
      blockSqs <- airportArrivalDelays * airportArrivalDelays
      blockSum <- sum(blockSqs, na.rm = TRUE) 
      #sum arrival delays for airport in current block
      blockMean <- mean(airportArrivalDelays, na.rm = TRUE)
      #mean arrival delays for airport in current block
      
      means[i] = mean(c(means[i],blockMean), na.rm = TRUE)
      squares[i] = squares[i] + blockSum
    } #while(TRUE)
    #need to calculate SD using mean & sum of squares
    sds[i] <- ((squares[i] - total[i] * means[i]^2)/(total[i]-1))^.5
  }#Loop over dests - at end will have totals, mean, and sum of squares
  
  output <- rbind(total, means, sds)
  return(output)
  
}#function end