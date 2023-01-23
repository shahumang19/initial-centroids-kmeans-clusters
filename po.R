myplot <- function(dta, cntr){
  
  #png(file = 'P:/College/Rollwala/Project/implementation/plot_check.png')
  plot(x = dta$x,y = dta$y,
       xlab = "X",
       ylab = "Y",
       xlim = c(1,10),
       ylim = c(1,10),		 
       main = "Plotting Check"
  )
  
  plot(x = cntr$x,y = cntr$y,
       xlab = "X",
       ylab = "Y",
       xlim = c(1,10),
       ylim = c(1,10),		 
       main = "Plotting Check",
       pch = 19,
       col = '#FF0000'
  )
  
  dev.off()
  
}

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

minD <- function(n){
  
  if(is.integer(n)){
    return(sqrt(2 * n))
  }
  stop("minD(n) : n must be an integer")
}

