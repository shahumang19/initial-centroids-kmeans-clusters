ssw <- function(data){
  
  if(!is.data.frame(data)){
    stop("Entered data should be like(list,dataframe).....")
  }
  
  centroid = getMeans(data)
  
  error = 0
  for(ix in c(1:nrow(data))){
    sum_of_squares = 0
    cx = data$cluster[ix]
    for(jx in c(1:(ncol(data)-1))){
      sum_of_squares = sum_of_squares + ((data[ix,jx] - centroid[[cx]][jx])^2)
    }
    sum_of_squares = sum_of_squares ^ 0.5
    error = error + sum_of_squares
  }
  print(error)
}

getMeans <- function(data){
  if(!is.data.frame(data)){
    stop("Entered data should be like(list,dataframe).....")
  }
  
  m = max(data$cluster)
  n = dim(data)[2]-1
  cnt = 1
  centroid = list()
  
  while(m >= cnt){
    centroid[[cnt]] = c(rep(0,n))
    cnt = cnt + 1
  }
  
  nrecords = nrow(data)
  cnt = 1
  
  while(cnt <= nrecords){
    row = data$cluster[cnt]
    centroid[[row]] = unlist(Map("+",centroid[[row]],data[cnt,1:n]))
    #cat(row," : ",centroid[[row]],"\n")
    cnt = cnt + 1
  }
  
  #print(centroid)
  
  cnt = 1
  
  while(cnt <= m){
    divisor = sum(data$cluster == cnt)
    centroid[[cnt]] = unlist(Map("/", centroid[[cnt]], divisor))
    cnt = cnt +1
  }
  return(centroid)
}