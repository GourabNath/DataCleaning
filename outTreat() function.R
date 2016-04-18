## Function for Outlier Treatment
## outTreat() function
##
## This function will help to treat the outliers. This function will take the data as an argument, detect the outlier with the help of a
## specified benchmark and will use a specified method - trimming or whinsorizing (as specified) to treat the outlier.
##
## The function thus has three arguments -
## data - the data frame of interest
## bench - the benchmark for outlier detection. It can take four specific benchmarks b1, b2, b3 & b4
##         b1: Based on quartile and interquartile range. (Upper BM, Lower BM) = (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
##         b2:  Based on mean and sd. (Upper BM, Lower BM) = (mean - 2*sd, mean + 2*sd)
##         b3:  Based on mean and sd. (Upper BM, Lower BM) = (mean - 2.5*sd, mean + 2.5*sd)
##         b4:  Based on mean and sd. (Upper BM, Lower BM) = (mean - 3*sd, mean + 3*sd)
## method - the method of treating the outlier. It can take two specific values
##         trim: for trimming. It will delete the cases containing outliers
##         whin: for whinsorizing. It will whinsorize the values of the variables that are outliers
##
##The function returns a outlier treated data.frame (i.e. a data set free of outliers)
##
##TRIMING METHOD IS UNDER CONSTRUCTION



outTreat <- function(data, benchmark = c("b1","b2","b3","b4") ,method = c("whin", "trim"))
{
  #Validation: The object data should be a data frame
  if(class(data) != "data.frame")
    stop("the object data should be a data.frame")
  
  #Extracting only the numerical variables
  for(i in 1:ncol(data))
  {
    if(class(data[,i]) == "factor")
    {
      data = data[,-i]
    }
  }
  
  if(benchmark == "b1")
  {
    #The benchmark is Benchmark used is (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
    
    if(method == "whin")
    {
      for(i in 1:ncol(data))
      {
        #Setting the upper and lower benchmark
        Lbench = quantile(data[,i][!is.na(data[,i])], 0.25) - 1.5*IQR(data[,i][!is.na(data[,i])])/2
        Ubench = quantile(data[,i][!is.na(data[,i])], 0.75) + 1.5*IQR(data[,i][!is.na(data[,i])])/2
        
        #Whinsorizing - replacing the outliers with the benchmark values
        data[,i][data[,i] > Ubench] <- Ubench
        data[,i][data[,i] < Lbench] <- Lbench
      }
    }
    
    #THIS PART OF THE PROGRAM IS UNDER CONSTRUCTION
    else if(method == "trim")
    {
      for(i in 1:ncol(data))
      {
        Lbench = quantile(data[,i][!is.na(data[,i])], 0.25) - 1.5*IQR(data[,i][!is.na(data[,i])])/2
        Ubench = quantile(data[,i][!is.na(data[,i])], 0.75) + 1.5*IQR(data[,i][!is.na(data[,i])])/2
        
        data[,i][data[,i] > Ubench] <- NA
        data[,i][data[,i] < Lbench] <- NA
      } 
      data <- data[complete.cases(data),]
    }
  }
  
  if(benchmark == "b2")
  {
    #Here the benchmark used is (Q1 - 2*SD, Q3 + 2*SD)
    
    if(method == "whin")
    {
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 2*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 2*sd(data[,i][!is.na(data[,i])])
        
        data[,i][data[,i] > Ubench] <- Ubench
        data[,i][data[,i] < Lbench] <- Lbench
      }
    }
    
    #THIS PART OF THE PROGRAM IS UNDER CONSTRUCTION
    else if(method == "trim")
    {
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 2*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 2*sd(data[,i][!is.na(data[,i])])
        
        data[,i][data[,i] > Ubench] <- NA
        data[,i][data[,i] < Lbench] <- NA
      } 
      data <- data[complete.cases(data),]
    }
  }
  
  
  if(benchmark == "b3")
  {
    #Here the benchmark used is (mean - 2.5*sd, mean + 2.5*sd)
    
    if(method == "whin")
    {
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 2*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 2*sd(data[,i][!is.na(data[,i])])
        
        data[,i][data[,i] > Ubench] <- Ubench
        data[,i][data[,i] < Lbench] <- Lbench
      }
    }
    
    #THIS PART OF THE PROGRAM IS UNDER CONSTRUCTION
    else if(method == "trim")
    {
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 2.5*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 2.5*sd(data[,i][!is.na(data[,i])])
        
        data[,i][data[,i] > Ubench] <- NA
        data[,i][data[,i] < Lbench] <- NA
      } 
      data <- data[complete.cases(data),]
    }
  }
  
  
  if(benchmark == "b4")
  {
    #Here the benchmark used is (mean - 3*sd, mean + 3*sd)
    
    if(method == "whin")
    {
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 3*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 3*sd(data[,i][!is.na(data[,i])])
        
        data[,i][data[,i] > Ubench] <- Ubench
        data[,i][data[,i] < Lbench] <- Lbench
      }
    }
    
    #THIS PART OF THE PROGRAM IS UNDER CONSTRUCTION
    else if(method == "trim")
    {
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 3*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 3*sd(data[,i][!is.na(data[,i])])
        
        data[,i][data[,i] > Ubench] <- NA
        data[,i][data[,i] < Lbench] <- NA
      } 
      data <- data[complete.cases(data),]
    }
  }
  
  return(data)
  
}
