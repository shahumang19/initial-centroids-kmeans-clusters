require(DMwR)
require(dplyr)
require(vegan)

km <-function(cen){
  cat("\n\nSelect data file : ")
  #File in CSV format 
  # Try Iris file with k=3
  fileName<<-file.choose()
  dir1<<-dirname(fileName)
  fname<<-basename(fileName)
  originalData<-read.csv(fileName,header=TRUE)
  c <-  kmeans(originalData,centers = cen)
  return(c)
}