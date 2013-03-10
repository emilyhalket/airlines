airportCountShell <-
function(files){

  cmd <- sprintf("LC_ALL=C cut -f 17 -d, %s.csv | egrep  'LAX|SFO|SMF|OAK' | sort | uniq -c", files)
  
  ans <- system(cmd, intern = TRUE)
  ans
}
