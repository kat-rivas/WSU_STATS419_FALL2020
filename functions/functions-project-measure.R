
prepareMeasureData = function(x)
{
  myData = select(measure, head.height, height, my.ethnicity, age, my.gender)
  myData$head.to.height = (myData$height - myData$head.height) / myData$head.height
  myData$ethnicity.groups <- NA
  
  for (i in 1:length(myData$my.ethnicity))
  {
    if (myData[i, 3] == "w")
    {
      myData[i, 7] = "1"  #1 for white
    }
    if (myData[i, 3] == "a" | myData[i, 3] == "j" | myData[i, 3] == "lao" |
        myData[i, 3] == "k" | myData[i, 3] == "c" | myData[i, 3] == "f" )
    {
      myData[i, 7] = "2"  #2 for asian,but not indian
    }
    if (myData[i, 3] == "al" | myData[i, 3] == "ca" | myData[i, 3] == "i" | myData[i, 3] == "ji" | 
        myData[i, 3] == "nat" | myData[i, 3] == "pi")
    {
      myData[i, 7] = "5"  #5 for other, i.e. mixed, indian, native american
    }
    if (myData[i, 3] == "b")
    {
      myData[i, 7] = "4"  #4 for african american
    }
    if (myData[i, 3] == "h" | myData[i, 3] == "l" )
    {
      myData[i, 7] = "3"  #3 for hispanic
    }
  }

  #remove NAs
  myData = na.omit(myData)
  
  #remove my.ethnicity because it's not numeric for correlation table
  myData = subset(myData, select = -my.ethnicity)
}