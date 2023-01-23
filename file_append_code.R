library("assertthat")

appendDataToFile <- function(x,data,fname_destination){
  # x is a list containing cluster numbers
  # data is the original data frame read directly from csv file
  # fname_destination is the name of the destination file where we want to write data
  
  if(is.list(x)){
    
    while(is.list(x[1])){
      x = x[[1]]
    }
    
    if(!is.integer(x[1])){
      stop("cluster numbers should be integers.....")
    }
    
  }else{
    stop("Entered data should be like(list,dataframe,string).....")
  }
  
  if(!is.string(fname_destination)){
    stop("Entered data should be like(list,dataframe,string).....")
  }
  
  if(!is.data.frame(data)){
    stop("Entered data should be like(list,dataframe,string).....")
  }
  
  # Adds column 
  data$cluster <- x
  write.csv(data, fname_destination, row.names = FALSE)
  
}