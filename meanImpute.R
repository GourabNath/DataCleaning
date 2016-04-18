
#Mean Imputation for specified column

meanImpute <- function(data, col = 1:ncol(data), trim)
{
  if(class(data) != "data.frame")
    stop("The data object should be a data frame")
    
  for(i in col)
   {
     if(class(data[,i]) != "factor")
       data[,i][is.na(data[,i])] = mean(data[,i], trim, na.rm = T)
     else
       stop("The variable should be numeric or integer")
   }
  
  return(data)
}



