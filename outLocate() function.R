## Locating Outliers:
## outLocate() function
##
## Outlier is definitely a matter of interest. Therefore it is required that we should know where exactly this outlier is located for a
## particular variable. This function will help to do so.
##
## The function outLocate takes two arguments - 
## data - a data frame of interest
## bench - the benchmark.
## There are four benchmarks used to construct this function:
## b1 - Based on quartile and interquartile range. (Upper BM, Lower BM) = (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
## b2 - Based on mean and sd. (Upper BM, Lower BM) = (mean - 2*sd, mean + 2*sd)
## b3 - Based on mean and sd. (Upper BM, Lower BM) = (mean - 2.5*sd, mean + 2.5*sd)
## b4 - Based on mean and sd. (Upper BM, Lower BM) = (mean - 3*sd, mean + 3*sd)
##
## The finction returns a list. Each location of the list corresponds to a variable in the data frame.
## If a particular variable of the data frame contains outliers then the corresponding location of the list returns a vector containing the index where 
## in the variable the outlier is located.
## If a particular variable of the data frame does not contain a outlier or a categorical variable then the corresponding location of the list
## shows a hypen (indicating not applicable)


outLocate <- function(data, benchmark = c("b1","b2","b3","b4"))
{
  #Extracting only the numerical variables
  indx <- c()
  for(i in 1:ncol(data))
  {
    if(class(data[,i]) == "factor")
    {
      data = data[,-i]
      
    }
  }
  
  #locating outliers: This list wil be used to store vectors of indices of each variables 
  #containing outliers
  outLoc <- list()
  
  
  if(benchmark == "b1")
  {
    #The benchmark is Benchmark used is (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
    
    for(i in 1:ncol(data))
    {
      #Constructing the upper and lower benchmark
      Lbench = quantile(data[,i][!is.na(data[,i])], 0.25) - 1.5*IQR(data[,i][!is.na(data[,i])])/2
      Ubench = quantile(data[,i][!is.na(data[,i])], 0.75) + 1.5*IQR(data[,i][!is.na(data[,i])])/2
      
      #Using which() function to locate the outliers
      a <- which(data[,i] > Ubench)
      b <- which(data[,i] < Lbench)
      
      #Note the if there are no outlier then the length of the vector c(a,b) will be zero
      if(length(c(a,b)) > 0)  #if outlier is present
      {
        outLoc[[i]] <- c(a,b) #Then the ith location of the list is the vector c(a,b)
      }
      else                    #Else If the outlier is not present
      {
        outLoc[[i]] <- "-"    #Then the ith location of the list is an hypen (indicating not applicable)
      }
    }
  }
      
      if(benchmark == "b2")
      {
        #Here the benchmark used is (mean - 2*sd, mean + 2*sd)
        
        for(i in 1:ncol(data))
        {
          Lbench = mean(data[,i][!is.na(data[,i])]) - 2*sd(data[,i][!is.na(data[,i])])
          Ubench = mean(data[,i][!is.na(data[,i])]) + 2*sd(data[,i][!is.na(data[,i])])
          
          a <- which(data[,i] > Ubench)
          b <- which(data[,i] < Lbench)
          
          if(length(c(a,b)) > 0)
          {
            outLoc[[i]] <- c(a,b)
          }
          else
          {
            outLoc[[i]] <- "-"
          }
          
        }
  
      }
  
    
    if(benchmark == "b3")
    {
      #Here the benchmark used is (mean - 2.5*sd, mean + 2.5*sd)
      
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 2.5*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 2.5*sd(data[,i][!is.na(data[,i])])
        
        a <- which(data[,i] > Ubench)
        b <- which(data[,i] < Lbench)
        
        if(length(c(a,b)) > 0)
        {
          outLoc[[i]] <- c(a,b)
        }
        else
        {
          outLoc[[i]] <- "-"
        }
        
      }
      
    }
    
    
    if(benchmark == "b4")
    {
      #Here the benchmark used is (mean - 3*sd, mean + 3*sd)
      
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 3*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 3*sd(data[,i][!is.na(data[,i])])
        
        a <- which(data[,i] > Ubench)
        b <- which(data[,i] < Lbench)
        
        if(length(c(a,b)) > 0)
        {
          outLoc[[i]] <- c(a,b)
        }
        else
        {
          outLoc[[i]] <- "-"
        }
        
      }
      
    }
  
  names(outLoc) <- names(data) #Naming the location of the list as the same as the variable names of the data frame.
  
  return(noquote(outLoc))   #noquote() function is used to remove quote when displaying a character variable.
}
