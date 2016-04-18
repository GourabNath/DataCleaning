##Outlier Detection
##outDetec() function
##The following function will hehp in detecting the presence of outliers in each of the numerical variables of a dataset given a specific
##benchmark. I have used four benchmarks (BM) while constructing this function. They are:
##b1 - Based on quartile and interquartile range. (Upper BM, Lower BM) = (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
##b2 - Based on mean and sd. (Upper BM, Lower BM) = (mean - 2*sd, mean + 2*sd)
##b3 - Based on mean and sd. (Upper BM, Lower BM) = (mean - 2.5*sd, mean + 2.5*sd)
##b4 - Based on mean and sd. (Upper BM, Lower BM) = (mean - 3*sd, mean + 3*sd)
##
##The function takes three arguments - 
##data - the data frame of interest
##bench - the benchmark
##plot - takes the value yes/no. if specified yes it will return the boxplot of the variables containing outliers
##
##The function returns a [ncol(data) x 1] matrix with its row names as the name of variables of the data set with its values indicating
##the presence of outliers.

outDetec <- function(data, bench = c("b1","b2","b3","b4"), plot = c("no","yes"))
{
  #Extracting only the numerical variables
  for(i in 1:ncol(data))
  {
    if(class(data[,i]) == "factor")
    {
      data = data[,-i]
    }
  }
  
  #Defining a col. matrix to store results
  out <- matrix(NA, ncol(data),1)
  
  if(bench == "b1")
  {
    #Detecting outliers
    for(i in 1:ncol(data))
    {
      #Defining the upper and lower benchmark
      #The benchmark is Benchmark used is (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
      Lbench = quantile(data[,i][!is.na(data[,i])], 0.25) - 1.5*IQR(data[,i][!is.na(data[,i])])/2
      Ubench = quantile(data[,i][!is.na(data[,i])], 0.75) + 1.5*IQR(data[,i][!is.na(data[,i])])/2
      
      if(length(data[,i][data[,i] < Lbench|data[,i] > Ubench]) > 0) #indicating presence of outliers
      {
        
        out[i,1] <- "Outlier Detected"
      }
      else
      {
        out[i,1] <- "-"
      }
    }
    a <- "Benchmark used is (Q1 - 1.5*IQR, Q3 + 1.5*IQR)"
    
  }
  else if(bench == "b2")
  {
    for(i in 1:ncol(data))
    {
      #Defining the upper and lower benchmark
      #The benchmark is Benchmark used is (mean - 2*sd, mean + 2*sd)
      Lbench = mean(data[,i][!is.na(data[,i])]) - 2*sd(data[,i][!is.na(data[,i])])
      Ubench = mean(data[,i][!is.na(data[,i])]) + 2*sd(data[,i][!is.na(data[,i])])
      
      if(length(data[,i][data[,i] < Lbench|data[,i] > Ubench]) > 0)
      {
        
        out[i,1] <- "Outlier Detected"
      }
      else #(length(data[,i][data[,i] < Lbench|data[,i] > Ubench]) == 0)
      {
        out[i,1] <- "-"
      }
    }
    a <- "Benchmark used is (mean - 2*sd, mean + 2*sd)"
    
  }
  
  else if(bench == "b3")
  {
    for(i in 1:ncol(data))
    {
      #Defining the upper and lower benchmark
      #The benchmark is Benchmark used is (mean - 2.5*sd, mean + 2.5*sd)
      Lbench = mean(data[,i][!is.na(data[,i])]) - 2.5*sd(data[,i][!is.na(data[,i])])
      Ubench = mean(data[,i][!is.na(data[,i])]) + 2.5*sd(data[,i][!is.na(data[,i])])
      
      if(length(data[,i][data[,i] < Lbench|data[,i] > Ubench]) > 0)
      {
        
        out[i,1] <- "Outlier Detected"
      }
      else #(length(data[,i][data[,i] < Lbench|data[,i] > Ubench]) == 0)
      {
        out[i,1] <- "-"
      }
  
    }
    a <- "Benchmark used is (mean - 2.5*sd, mean + 2.5*sd)"
    
  }
  
  else if(bench == "b4")
  {
    for(i in 1:ncol(data))
    {
      #Defining the upper and lower benchmark
      #The benchmark is Benchmark used is (mean - 3*sd, mean + 3*sd)
      Lbench = mean(data[,i][!is.na(data[,i])]) - 3*sd(data[,i][!is.na(data[,i])])
      Ubench = mean(data[,i][!is.na(data[,i])]) + 3*sd(data[,i][!is.na(data[,i])])
      
      if(length(data[,i][data[,i] < Lbench|data[,i] > Ubench]) > 0)
      {
        
        out[i,1] <- "Outlier Detected"
      }
      else #(length(data[,i][data[,i] < Lbench|data[,i] > Ubench]) == 0)
      {
        out[i,1] <- "-"
      }
      
    }
    a <- "Benchmark used is (mean - 3*sd, mean + 3*sd)"
  }
  
  
  rownames(out) <- names(data)
  colnames(out) <- "Status"
  
  if(plot == "yes")
  {
    for(i in 1:nrow(out))
    {
      if(out[i,1] == "Outlier Detected")
      {
        boxplot(data[,i], main = names(data)[i])
      }
    }
  }
  list <- list(outlier.status = out, note = a)
  return(noquote(list))
 
}  

outDetec(wine, "b3", "no")
