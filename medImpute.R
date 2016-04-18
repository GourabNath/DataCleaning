## Median Imputation for specified column
## Function: medImpute()
## The function medImpute allows imputation of missing values using the median of a variable. The function takes two arguments - 
## data - a data frame of interest
## col - a vector specifying the columns for which imputation is needed to be done
##
##The function returns a data frame with its after the median imputation for the specified variables.

medImpute <- function(data, col = 1:ncol(data))
{
  validation: The object class sould be a data frame
  if(class(data) != "data.frame")
    stop("The data object should be a data frame")
  
  for(i in col)
  { 
    #Validation: The variable should not be a factor variable
    if(class(data[,i]) != "factor")
      data[,i][is.na(data[,i])] = median(data[,i], na.rm = T)
    else
      stop("The variable should be numeric or integer")
  }
  return(data)
}
