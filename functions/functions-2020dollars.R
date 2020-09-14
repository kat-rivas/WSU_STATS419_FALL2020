# helperDeflationRate
#
# this will divide two row elements
# with each other
#
# @param yi, row element
# @param yf, row element 
# @return val, the rate of change one year
helperDeflationRate<-function(yi,yf)
{
  val<-yi/yf
  return(val)
}

# getDeflationRate
# 
# gets the deflation rate of change
# one year at a time across 
# a number of years
# 
# @param x, vector of change in dollar 
#	valuation from one year to the next
# @return f, vector of the ratio of change 
getDeflationRate<-function(x)
{
  f<-c()
  for (i in 1:nrow(x))
  {
    xi<-x[i, "dollar"]
    xf<-x[i+1, "dollar"]
    ratio <- helperDeflationRate(xi,xf)
    f<-append(f, ratio)
  }
  f
}

# getRate2020
#
# finds the percent increase using 
# a vector that was calculated to find 
# the year position of will/denzel film
# to the inflation table
#
# @param x, vector of will/denzel years in 
#	relation to inflation table
# @return final, vector of adjusted to 2020 value
getRate2020<-function(x)   #use m to find the percent increase from that m position in inflation
{                #to end of inflation table 
  final<-c()
  
  for (i in x) 
  {
    adjust<-cumprod(inflation$adjusted[i:101])
    last<-last(adjust)
    final<-append(final, last)
  }
  final
}
