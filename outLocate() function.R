#Locating Outliers:
#outLocate() function

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
      Lbench = quantile(data[,i][!is.na(data[,i])], 0.25) - 1.5*IQR(data[,i][!is.na(data[,i])])/2
      Ubench = quantile(data[,i][!is.na(data[,i])], 0.75) + 1.5*IQR(data[,i][!is.na(data[,i])])/2
      
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
      
      if(benchmark == "b2")
      {
        #Here the benchmark used is (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
        
        for(i in 1:ncol(data))
        {
          Lbench = mean(data[,i][!is.na(data[,i])]) - 1.5*sd(data[,i][!is.na(data[,i])])
          Ubench = mean(data[,i][!is.na(data[,i])]) + 1.5*sd(data[,i][!is.na(data[,i])])
          
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
      #Here the benchmark used is (Q1 - 2*IQR, Q3 + 2*IQR)
      
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
    
    
    if(benchmark == "b4")
    {
      #Here the benchmark used is (Q1 - 3*IQR, Q3 + 3*IQR)
      
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
  
  names(outLoc) <- names(data)
  
  return(noquote(outLoc))
}
