distance <- name <- function(v1, c) {
  #Euclidean Distance
  ans = 0
  if ((typeof(v1) == 'list') && (typeof(c) == 'list') && (length(v1) == length(c))){
    for (indx in 1:length(v1)) {
      
      ans <- ans + ((v1[[indx]] - c[[indx]])^2)
      
    }
    
    return(ans ^ 0.5)
    
  }
  
  return(-1)
}

getdata <- function(){
  cat("\nSelect data file : ")
  filename <- file.choose()
  header <- readline("\nDoes the file contain header(y/n) : ")
  if(header == "y"){
    header = TRUE
  }
  else if(header == "n"){
    header = FALSE
  }
  else{
    stop("\n\nWrong input.....")
  }
  csvdata <- read.csv(filename,header = header)
  return(csvdata)
}

allocate_data_points <- function(MinMax, dataPoint){
  if(!(is.data.frame(MinMax) & is.data.frame(dataPoint))){
    stop("MinMax and dataPoint should be data frames.....")
  }
  
  if(nrow(dataPoint) == 0 | nrow(MinMax) == 0){
    #print(dataPoint)
    #print(MinMax)
    return(NULL)
    #stop("Empty dataframes are not allowed.....")
  }
  
  bin=dataPoint[FALSE, ]
  
  ix = 1
  n = nrow(dataPoint)
  
  while(ix <= n){
    Min = distance(MinMax[1,], dataPoint[ix,])
    mid = distance(MinMax[2,], dataPoint[ix,])
    max = distance(MinMax[3,], dataPoint[ix,])
    
    if(Min >= mid | max >= mid){
      bin = rbind(bin, dataPoint[ix,])
    }
    ix = ix + 1
  }
  #print(bin)
  return(bin)
}

extractData <- function(Min,Max,dataPoint){
  if(!(is.list(Min) & is.list(Max) & is.data.frame(dataPoint))){
    stop("Data should be entered in (list,list,data.frame) format .....")
  }
  
  #print("Min")
  #print(Min[1])
  #print("Max")
  #print(Max[1])
  
  ix = 1
  n = nrow(dataPoint)
  m = ncol(dataPoint)
  acceptable = list()
  newData = dataPoint[FALSE, ]
  
  while(ix <= n){
    jx = 1
    
    while (jx <= m) {
      if(dataPoint[ix,jx] >= Min[jx] & dataPoint[ix,jx] <= Max[jx]){
        acceptable[[jx]] = TRUE
      }
      else{
        acceptable[[jx]] = FALSE
        break
      }
      jx = jx + 1
    }
    
    if( all(unlist(acceptable)) ){
      newData = rbind(newData, dataPoint[ix, ])
    }
    
    ix = ix + 1
  }
  return(newData)
  
}

densityOfBin <- function(bin){
  if(!is.data.frame(bin)){
    if(is.null(bin)){return(0)}
    stop("bin should be a dataframe.....")
  }
  return (nrow(bin))
}

radius <- function(MinMax,dataPoint){
  #print(MinMax)
  dataPoint = extractData(MinMax[1,],MinMax[3,],dataPoint)
  bin = allocate_data_points(MinMax, dataPoint)
  return(bin)
}
