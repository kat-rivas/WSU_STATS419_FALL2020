# doSampleVariance
#
# computes sample variance depending on
# user input to use naive or two pass algorithm
#
# @param x vector
# @param method string, either "naive" or other
# @return the sample variance for the vector/data
doSampleVariance = function(x, method)
{
  if(method=="naive")
  {
    count<-0
    sum<-0
    sumsq<-0
    for (val in x) {
      count<-count+1
      sum<-sum+val
      sumsq<-sumsq+val*val
    }
    var<-(sumsq-(sum*sum)/count)/(count-1) 
    summary<-c("sum"=sum, "sumSq"=sumsq,
               "var"=var)
    summary
    
  }
  else
  {
    # two-pass algorithm
    n<-0
    sum1<-0
    sum2<-0
    for (val in x) {
      n<- n+1
      sum1<-sum1+val
    }
    
    mean<-sum1/n
    
    for (val in x) {
      sum2<-sum2+((val-mean)^2)
    }
    var<-sum2 / (n-1)
    
    result<-c("sum"=sum1, "sum2"=sum2, "var"=var)  
    result
  }
}

# doMode
#
# finds the mode using a frequency table
# and max functions
#
# @param x vector
# @return mode
doMode = function(x)
{
  # This function finds highest frequencies
  # using frequency table() and max()
  
  t<-table(x)
  xmax<-max(t)
  t; xmax
  
  if (all(t == xmax))
  {
    mode = NA
  } else if (is.numeric(x))
  {
    mode = as.numeric(names(t)[t == xmax])
  } else
  {
    mode = names(t)[t == xmax]
  }
  mode
}

# doSummary
#
# summary statistics for a given vector
# - number of data elements
# - number of NAs
# - mean
# - median
# - mode
# - variance, set to "naive"
# - standard deviation
# - standard deviation as sqrt(variance)
#
# @param x vector
# @return statistics on the vector
#
doSummary = function(x)
{
  # length
  length = length(x); length
  # number of NAs
  numNAs =sum(is.na(x)); numNAs
  # mean
  mean = mean(x)
  # median
  median=median(x)
  # mode 
  mode=doMode(x)
  # variance 
  var = doSampleVariance(x, "naive")
  # sd ... built in function but compare it to the custom function ... 
  r_sd<-sd(x)
  custom_sd<-sqrt(var[3])
  
  #summary
  summary<-c("length"=length, "number of NAs"=numNAs, 
             "mean"=mean, "median"=median, "mode"=mode,
             "var"=var, "built-in sd"=r_sd, "custom sd"=custom_sd)
  
  summary
}


# doZScore
#
# z-score for a given vector
# @param x vector
# @return z-scores on the vector
#
doZScore=function(x)
{
  mean<-mean(x)
  sd<-sd(x)
  z<-(x-mean)/sd
  z
}