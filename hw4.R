cmd = "for i in %s; do
      echo $ i
       egrep $i %s*.csv | wc -l
       done"

ans = system(sprintf( cmd, "LAX SFO SMF OAK", "[1]"), intern = TRUE)

#in shell

echo 1987.csv | cut -f 17 -d, 1987.csv | grep LAX 1987.csv | wc -l


for i in LAX OAK SMF SFO; do
echo $ | cut -f 17 -d, 