#setwd("D:\\Umang\\College\\MScAI\\SEM-I\\Project")
source("po.R")
source("functions.R")
source("algorithm.R")

main <- function(){
  originalData = getdata()
  MinD = minD(nrow(originalData))
  print(MinD)
  initialCentroid = NULL
  check = TRUE
  iterations = 1
  
  while(check){
    print(iterations)
    check = FALSE
    Summary = Quartile(originalData)
    #print(Summary[1:3,])
    cnt = 1
    bin = list()
    while(cnt < 4){
      #print(originalData)
      bin[[cnt]] = radius(Summary[cnt:(cnt + 2), ], originalData)  
      cnt = cnt + 1
    }
    cnt = 1
    #print(MinD)
    print(bin)
    while(cnt<length(bin)){
      if(densityOfBin(bin[[cnt]]) >= MinD){
        originalData = remove(originalData,bin[[cnt]])
        initialCentroid = rbind(initialCentroid,Summary[cnt+1,])
        check = TRUE
      }
      cnt = cnt + 1
    }
    iterations = iterations + 1
  }
  print("Initial Centroids : ")
  print(initialCentroid)
  return(initialCentroid)
}