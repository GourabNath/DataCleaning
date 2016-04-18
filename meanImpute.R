
## Mean Imputation for specified columns
## Function: meanImpute()
##
## This function will help us to impute the missing values with mean/trimmed mean specific columns.
## The function takes three arguments - 
## data - the data frame of interest
## col - a vector taking the index of the columns that are needed to be imputed for. By default it takes all the columns.
## trim - A value between 0 - 1. Allows missing value imputation using trimmed mean. By default it is set to zero.
##
## The function returns a data frame with the nissing values of certain variables imputed with mean/trimed mean of the variables specified. 

meanImpute <- function(data, col = 1:ncol(data), trim = 0)
{
  #Validation: The object data should be a data frame
  if(class(data) != "data.frame")
    stop("The data object should be a data frame")
    
  for(i in col)
   {
     #Validation: The variable mentioned should not be a factor
     if(class(data[,i]) != "factor")
     
     #Imputing missing value for the ith variable with trimmed mean with the NA values removed.
       data[,i][is.na(data[,i])] = mean(data[,i], trim, na.rm = T)
     else
       stop("The variable should be numeric or integer")
   }
  
  return(data)
}



