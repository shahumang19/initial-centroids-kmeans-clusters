Quartile <- function(data){
  cols <- ncol(data)
  summary <- NULL
  cnt = 1
  while(cols){
    quartile= quantile(data[,cols])
    summary = cbind(summary,quartile)
    cols = cols - 1
  }
  rownames(summary) <- c()
  colnames(summary) <- c()
  return(as.data.frame(summary))
}

remove <- function(data, bin){
  ix = 1
  n = if(is.null(bin)) 0 else nrow(bin)
  
  while(ix < n)
  {
    jx = 1
    m = if(is.null(data)) 0 else nrow(data)
    while(jx < m)
    {
      #print(bin[ix,])
      #print(data[jx,])
      if(all(bin[ix,] == data[jx,]))
      {
        data = data[-jx,]
        m = m - 1
      }
      jx = jx + 1
    }
    ix = ix + 1
  }
  return(data)
}
