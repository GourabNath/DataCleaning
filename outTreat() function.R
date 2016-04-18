#Function for Outlier Treatment
#outTreat() function

outTreat <- function(data, benchmark = c("b1","b2","b3","b4") ,method = c("whin", "trim"))
{
  
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
        Lbench = quantile(data[,i][!is.na(data[,i])], 0.25) - 1.5*IQR(data[,i][!is.na(data[,i])])/2
        Ubench = quantile(data[,i][!is.na(data[,i])], 0.75) + 1.5*IQR(data[,i][!is.na(data[,i])])/2
        
        data[,i][data[,i] > Ubench] <- Ubench
        data[,i][data[,i] < Lbench] <- Lbench
      }
    }
    
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
    #Here the benchmark used is (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
    
    if(method == "whin")
    {
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 1.5*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 1.5*sd(data[,i][!is.na(data[,i])])
        
        data[,i][data[,i] > Ubench] <- Ubench
        data[,i][data[,i] < Lbench] <- Lbench
      }
    }
    
    else if(method == "trim")
    {
      for(i in 1:ncol(data))
      {
        Lbench = mean(data[,i][!is.na(data[,i])]) - 1.5*sd(data[,i][!is.na(data[,i])])
        Ubench = mean(data[,i][!is.na(data[,i])]) + 1.5*sd(data[,i][!is.na(data[,i])])
        
        data[,i][data[,i] > Ubench] <- NA
        data[,i][data[,i] < Lbench] <- NA
      } 
      data <- data[complete.cases(data),]
    }
  }
  
  
  if(benchmark == "b3")
  {
    #Here the benchmark used is (Q1 - 2*IQR, Q3 + 2*IQR)
    
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
  
  
  if(benchmark == "b4")
  {
    #Here the benchmark used is (Q1 - 3*IQR, Q3 + 3*IQR)
    
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
