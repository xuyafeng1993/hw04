#排序
my.sort <- function(x,decrease=TRUE){
  a <- x
  if(decrease==TRUE){
    for(i in 1:(length(x)-1)){
      for(j in (i+1):length(x)){
        if(x[i]<x[j]){
          temp <- x[i]
          x[i] <- x[j]
          x[j] <- temp
        }
      }
    }
  }
  else{
    for (i in 1:(length(x)-1)){
      for(j in (i+1):length(x)){
        if(x[i]>x[j]){
          temp <- x[i]
          x[i] <- x[j]
          x[j] <- temp
        }
      }
    }
  }
  return(list(x))
}
# input data
x <- runif(10)
# function calls
my.sort(x,decrease =FALSE)


#标准差
# self defined functions
my.SD <- function(x){
  df <- length(x)-1
  n <- length(x)
  mean.x <- mean(x)
  square <- function(x){
    a <- x^2
    return(a)
  }
  x <- as.matrix(x)
  r <- sum(apply(x,square,MARGIN = 1))
  variance <- (r-n*mean.x^2)/df
  SD <- sqrt(variance)
  return(SD)
}
# input data
a <- 1:6
# function calls
sqrt(var(a))