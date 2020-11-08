library(dplyr)

prepareMeasureData = function(x)
{
  subset = select(measure, head.height, height, my.ethnicity, age, my.gender)
  subset$head.to.height = (subset$height - subset$head.height) / subset$head.height
  subset$ethnicity.groups <- NA
  
  for (i in 1:length(subset$my.ethnicity))
  {
    if (subset[i, 3] == "w")
    {
      subset[i, 7] = "w"  #w for white
    }
    if (subset[i, 3] == "a" | subset[i, 3] == "j" | subset[i, 3] == "lao" |
        subset[i, 3] == "k" | subset[i, 3] == "c" | subset[i, 3] == "f" )
    {
      subset[i, 7] = "a"  #a for asian,but not indian
    }
    if (subset[i, 3] == "al" | subset[i, 3] == "ca" | subset[i, 3] == "i" | subset[i, 3] == "ji" | 
        subset[i, 3] == "nat" | subset[i, 3] == "pi")
    {
      subset[i, 7] = "o"  #o for other, i.e. mixed, indian, native american
    }
    if (subset[i, 3] == "b")
    {
      subset[i, 7] = "b"  #b for african american
    }
    if (subset[i, 3] == "h" | subset[i, 3] == "l" )
    {
      subset[i, 7] = "h"  #h for hispanic
    }
  }
  
  #remove NAs
  subset = na.omit(subset)
}