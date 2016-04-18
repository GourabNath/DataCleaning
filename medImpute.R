#Median Imputation for specified column

medImpute <- function(data, col = 1:ncol(data))
{
  if(class(data) != "data.frame")
    stop("The data object should be a data frame")
  
  for(i in col)
  {
    if(class(data[,i]) != "factor")
      data[,i][is.na(data[,i])] = median(data[,i], na.rm = T)
    else
      stop("The variable should be numeric or integer")
  }
  return(data)
}