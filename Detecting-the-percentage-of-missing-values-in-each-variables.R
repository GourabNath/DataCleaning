
#Detecting the percebtage of missing value in each variables

missDetect <- function(data)
{
  if(class(data) != "data.frame")
    stop("The object data should be a data.frame")
  
  mat <- matrix(NA, ncol(data), 1)
  
  for(i in 1:ncol(data))
  {
    mat[i,1] <- sum(is.na(data[,i]))/nrow(data)*100
  }
  
  rownames(mat) <- names(data)
  colnames(mat) <- "%-missing"
  
  return(round(mat,4))
}



missDetect(co2)
