#
# Author:   Cristian E. Nuno
# Purpose:  Download raw data sets from ISLR page
# Date:     January 21, 2019
#

# all data comes from this webpage http://www-bcf.usc.edu/~gareth/ISL/data.html

# store raw data file locations ----
raw.csv.files <-
  list("advertising.csv" = "http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv"
       , "ch10_ex11_.csv" = "http://www-bcf.usc.edu/~gareth/ISL/Ch10Ex11.csv"
       , "income1.csv" = "http://www-bcf.usc.edu/~gareth/ISL/Income1.csv"
       , "income2.csv" = "http://www-bcf.usc.edu/~gareth/ISL/Income2.csv"
       , "heart.csv" = "http://www-bcf.usc.edu/~gareth/ISL/Heart.csv")

# download them all into 00_raw_data/ ----
mapply(FUN = function(i, j) 
  download.file(i, destfile = file.path("00_raw_data", j))
  , raw.csv.files
  , names(raw.csv.files))

# end of script #
