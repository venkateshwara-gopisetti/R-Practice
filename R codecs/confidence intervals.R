ci <- function(mean ,sd, n=1, level){
  if (level == 90){
    lower <- 1.645*sd/sqrt(n)
  }
  else if(level == 95){
    lower <- 1.962*sd/sqrt(n)
  }
  else{
    lower <- 2.576*sd/sqrt(n)
  }
  return(c(mean-lower,mean+lower))
}