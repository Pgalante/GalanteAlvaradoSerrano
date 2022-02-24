#FUNCTION MODIFIED FROM script by Michael Scroggie
#a function to select N points on a raster, with
#inclusion probabilities defined by the raster values
#while avoiding the issue of the frequency of different cell
#values affecting the representativity of different values
#in final samples.
probsel<-function(probrast, N){
  require(raster); require(sp)
  x<-getValues(probrast)
  #set NA cells in raster to zero
  x[is.na(x)]<-0
  samp<-sample(nrow(probrast)*ncol(probrast), size=N, prob=x^1)
  samprast<-raster(probrast)
  samprast[samp]<-1 #set value of sampled squares to 1
  #convert to SpatialPoints
  points<-rasterToPoints(samprast, fun=function(x){x>0})
  points<-SpatialPoints(points)
 return(points)
}

