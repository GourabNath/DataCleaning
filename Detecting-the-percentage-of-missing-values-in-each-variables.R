
#Detecting the percebtage of missing value in each variables

missDetect <- function(data)
{
  #validation: The argument of the function should be a data frame
  if(class(data) != "data.frame")
    stop("The object data should be a data.frame")
  
  #Creating a matrix of dim ncol(data) x 1 
  #We will store the info of missing data for each variables in each rows of the matrix 
  mat <- matrix(NA, ncol(data), 1)
  
  for(i in 1:ncol(data))
  {
    #Calculating the % of missing values for the ith variable
    mat[i,1] <- sum(is.na(data[,i]))/nrow(data)*100
  }
  
  #setting the row names of the matrix as equal to the names of the variables in the data
  rownames(mat) <- names(data)
  #The columns will store %-age of missing values and hence name it "%-missing"
  colnames(mat) <- "%-missing"
  
  #Return the matrix by rounding of each variables to 4 decimal places.
  return(round(mat,4))
}

