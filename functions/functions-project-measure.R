
prepareMeasureData = function(x)
{
  subset.df = select(x, head.height, height, my.ethnicity, age, my.gender)
  subset.df$head.to.height = (subset.df$height - subset.df$head.height) / subset.df$head.height
  subset.df$ethnicity.groups <- NA
  
  for (i in 1:length(subset.df$my.ethnicity))
  {
    if (subset.df[i, 3] == "w")
    {
      subset.df[i, 7] = "1"  #1 for white
    }
    if (subset.df[i, 3] == "a" | subset.df[i, 3] == "j" | subset.df[i, 3] == "lao" |
        subset.df[i, 3] == "k" | subset.df[i, 3] == "c" | subset.df[i, 3] == "f" )
    {
      subset.df[i, 7] = "a"  #2 for asian,but not indian
    }
    if (subset.df[i, 3] == "al" | subset.df[i, 3] == "ca" | subset.df[i, 3] == "i" | subset.df[i, 3] == "ji" | 
        subset.df[i, 3] == "nat" | subset.df[i, 3] == "pi")
    {
      subset.df[i, 7] = "5"  #5 for other, i.e. mixed, indian, native american
    }
    if (subset.df[i, 3] == "b")
    {
      subset.df[i, 7] = "4"  #4 for african american
    }
    if (subset.df[i, 3] == "h" | subset.df[i, 3] == "l" )
    {
      subset.df[i, 7] = "3"  #3 for hispanic
    }
  }
  
  for (j in 1:length(subset.df$my.gender))
  {
    if (subset.df[i,5] == "f")
    {
      subset.df[i,5] == "1"
    }
    if (subset.df[i,5] == "m")
    {
      subset.df[i,5] == "2"
    }
    if (subset.df[i,5] == "o")
    {
      subset.df[i,5] == "3"
    }
  }
  
  #remove NAs
  subset.df = na.omit(subset)
  
  #remove my.ethnicity because it's not numeric for correlation table
  subset.df = subset[subset.df ,select = -c(my.ethnicity)]
}