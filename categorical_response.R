####Written by DFA Winter 2014-15
cat.response = function(y,cat1,suit1,cat2,suit2){
  # overly simple function to recreate a catagorical variable response 
  #for maxent
  # y: values onto which you apply the function below, should be a vector
  # cat1: number of category bin 1
  # suit1: suitability of categorical bin 1
  # cat2: number of category bin 2
  # suit2: suitability of categorical bin 2
  print(sum(cat1,suit1,cat2,suit2))
  suitvals = y
  for (i in 1:length(y)){
    print (y[i])
    if (is.na(y[i])){
      suitvals[i] = NA
    } else {
      if (y[i] == cat1){
        suitvals[i] = suit1
      } else { if (y[i] == cat2){
        suitvals[i] = suit2
      } else{
        suitvals[i] = 0
      }}}}
  return(suitvals)
}
