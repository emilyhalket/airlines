

#read in blocks using str.split (loop?) and calculate mean & sd

arrivalDelayStats<- function( origin = c("LAX", 'SMF', 'OAK', 'SFO'), files, B){
  #set con
  
  
  
  total <- structure(integer(length(origin)), names = origin)
  #keep track of total flights for airport
  
  means <- structure(integer(length(origin)), names = origin)
  #keep track of mean delay
  means[] <- NA  #set to NA so when I take mean later I don't have 0 involved
  squares <- structure(integer(length(origin)), names = origin)
  #keep track of sqaures ( to sum ) for airport
  
  sds <- structure(integer(length(origin)), names = origin) 
  for (i in 1:length(origin)){ #loop over airports
    
    grepCmdOrigin <- sprintf("grep %s %s", origin[i], files)
    con = pipe(grepCmdOrigin, "r")
    
    
    while(TRUE){
      
      
      ll <- readLines(con, n=B)  
      #because my file is open, I can just pick up where i left off in loop
      # i don't want to use[-numberISaw] because then I read it only to throw it away
      if(length(ll) ==0)
        break
      
      originSplit = sapply(strsplit(ll, ","), '[[', 17)
      # array of origins
      arrDelaySplit = sapply(strsplit(ll, ","), '[[', 15)
      #array of arrival delays
      airportOrigin = originSplit == origin[i]
      #find where origin equals airport of interest
      originDelays = arrDelaySplit[airportOrigin]
      #index delays for airports of interest
      airportArrivalDelays = as.numeric(originDelays) #if contains header --> NA value
      
      #take care of NAs?
      
      
      subcounts <- sum(airportOrigin)#counts what I want by origin
      #subcounts[is.na(subcounts)] = 0
      
      total[[i]] <-  total[[i]] + subcounts #update total num of flights/origin
      
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
  }#Loop over origins - at end will have totals, mean, and sum of squares
  
  output <- rbind(total, means, sds)
  return(output)
  
}#function end