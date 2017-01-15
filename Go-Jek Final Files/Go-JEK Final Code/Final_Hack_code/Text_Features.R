#text1 = c("I am looking for house at kormngala for 85 lakhs")
#text2 = c(" I am looking for house in east bangalore marthli for 90 lakhs")
#text3 = c(" I am looking for apartment in west bangalore for 75 lakhs")
#text4 = "hi"

library(tm)
library(stringdist)
library(Hmisc)
require(readxl)

mapfunction <- function(searchlist, reflist){
  
  sizewords <- dim(searchlist)[1]
  matchdist <- NULL
  close_loc <- NULL
  pmatch_loc <- NULL
  
  Location_match = 0
  for(i in 1:sizewords)
  {
    dist <- stringdist(searchlist[i],reflist, method='jw')
    mindist_ind <- which(dist==min(dist))[1];
    matchdist[i] <- min(dist)
    close_loc[i] <- reflist[mindist_ind]
  }
  
  if(min(matchdist)<0.25){
  Key_location <- close_loc[which(matchdist==min(matchdist))[1]]
  }
  else{
    Key_location <- NULL
  }
  return(Key_location)
}