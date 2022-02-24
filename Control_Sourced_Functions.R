#install.packages('ENMeval')
#install.packages('virtualspecies')
#install.packages('doParallel')
library(ENMeval)
library(virtualspecies)
library(doParallel)
rm(list=ls())
#####These need to be loaded in beforehand
#setwd('C:/Users/pgalante/Documents/Projects/PJG/fake_LCvars')
#LCBio<-stack(list.files(pattern='\\.tif$'))
#corr.list<- read.csv('C:/Users/pgalante/Documents/Projects/PJG/fake_LC.65.csv', header=T)
#####These functions all need to be sourced
# source('C:/Users/pgalante/Documents/Projects/PJG/Function_to_source/simulation_functions_to_source.R')
# source('C:/Users/pgalante/Documents/Projects/PJG/Function_to_source/optimization_functions_to_source.R')
# source('C:/Users/pgalante/Documents/Projects/PJG/Function_to_source/ENMEVAL.R')
# source('C:/Users/pgalante/Documents/Projects/PJG/Function_to_source/TUNER.R')
# source("C:/Users/pgalante/Documents/Projects/PJG/Function_to_source/categorical_response.R")
# source("C:/Users/pgalante/Documents/Projects/PJG/Function_to_source/sample_raster_by_prob.R")
# source("C:/Users/pgalante/Documents/Projects/PJG/Function_to_source/variable_removal.R")

setwd('variableimportance/15Locs/functions')
for (i in c(1,3:length(list.files(full.names = T, pattern = '\\.R$')))){
  source(list.files(full.names = T, pattern = '\\.R$')[[i]])
}
### Define categorical variable
cat.vars='geosimp'
###### This will create the niche and localities
createniche(envdir='/Env', wd='/variableimportance/15Locs',
            packages = c('ENMeval', 'virtualspecies', 'rgdal', 'maptools', 'foreach','doParallel'), 
            nsets=100, download=F, cont.vars=c('bio1', 'bio12'), 
            cat.vars, means=c(260, 1400),
            SDs= c(50, 500), cats=c(6, 7), cat.vals=c(0.7,0.3), backg=NULL,
            res=0.5, long=43.00, lats=-26.5, locsdir='/variableimportance/15Locs/locs',
            max.corr=0.75, numlocs=15)

###### Define which variables are categorical
cat.vars<- 'geosimp'
setwd('..')
dir.create('rasters')
dir.create('rasters/s1')
dir.create('rasters/s2')
dir.create('rasters/s3')
dir.create('rasters/s4')
dir.create('results')
dir.create('D_values')
dir.create('varsused')

###### This will perform optimization on all simulated locality datasets
for (i in 1:100){
  DO.OPTIMIZATION(locs= all.locs[[i]], env= env, method= 'jackknife', categoricals= cat.vars, fc= c("L", "LQ", "H", "LQH"), bg.coords= bg.coords, RMvalues=seq(1, 4, 0.5), cat.vars)
}

setwd('..')
dir.create('rasters')
dir.create('rasters/s1')
dir.create('rasters/s2')
dir.create('rasters/s3')
dir.create('rasters/s4')
dir.create('results')
dir.create('D_values')

for (i in 1:nlayers(s1)){
  writeRaster(s1[[i]], filename = paste(paste(getwd(), 'rasters/s1', sep='/'),paste('S',i, names(s1[[i]]),sep=''), sep='/'), format='GTiff')
}
for (i in 1:nlayers(s2)){
  writeRaster(s2[[i]], filename = paste(paste(getwd(), 'rasters/s2', sep='/'),paste('S',i, names(s2[[i]]),sep=''), sep='/'), format='GTiff')
}
for (i in 1:nlayers(s3)){
  writeRaster(s3[[i]], filename = paste(paste(getwd(), 'rasters/s3', sep='/'),paste('S',i, names(s3[[i]]),sep=''), sep='/'), format='GTiff')
}
for (i in 1:nlayers(s4)){
  writeRaster(s4[[i]], filename = paste(paste(getwd(), 'rasters/s4', sep='/'),paste('S',i, names(s4[[i]]),sep=''), sep='/'), format='GTiff')
}
write.csv(resA, file = paste(getwd(), 'results/resA.csv', sep='/'))
write.csv(resB, file = paste(getwd(), 'results/resB.csv', sep='/'))
write.csv(resC, file = paste(getwd(), 'results/resC.csv', sep='/'))
write.csv(resD, file = paste(getwd(), 'results/resD.csv', sep='/'))
write.csv(D1, file = paste(getwd(), 'D_values/D1.csv', sep='/'))
write.csv(D2, file = paste(getwd(), 'D_values/D2.csv', sep='/'))
write.csv(D3, file = paste(getwd(), 'D_values/D3.csv', sep='/'))
write.csv(D4, file = paste(getwd(), 'D_values/D4.csv', sep='/'))
writeLines(unlist(lapply(D1, paste, collapse = '')),'D_values/D1.txt')
writeLines(unlist(lapply(D2, paste, collapse = '')),'D_values/D2.txt')
writeLines(unlist(lapply(D3, paste, collapse = '')),'D_values/D3.txt')
writeLines(unlist(lapply(D4, paste, collapse = '')),'D_values/D4.txt')

save.image(file = "Rspace.rdata")

res1A<-list()
res1A<-resA[[1]]
for (i in 2:100){
  res1A<-rbind(res1A, resA[[i]])
}
write.csv(res1A, '/variableimportance/15Locs/results/res1A.csv')

res1B<-list()
res1B<-resB[[1]]
for (i in 2:100){
  res1B<-rbind(res1B, resB[[i]])
}
write.csv(res1B, '/variableimportance/15Locs/results/res1B.csv')

res1C<-list()
res1C<-resC[[1]]
for (i in 2:100){
  res1C<-rbind(res1C, resC[[i]])
}
write.csv(res1C, '/variableimportance/15Locs/results/res1C.csv')

res1D<-list()
res1D<-resD[[1]]
for (i in 2:100){
  res1D<-rbind(res1D, resD[[i]])
}
write.csv(res1D, '/variableimportance/15Locs/results/res1D.csv')

D1s<-list()
D1s<-D1[[1]]
for (i in 2:100){
  D1s<-rbind(D1s, D1[[i]])
}
write.csv(D1s, '/variableimportance/15Locs/D_values/D1s.csv')

D2s<-list()
D2s<-D2[[1]]
for (i in 2:100){
  D2s<-rbind(D2s, D2[[i]])
}
write.csv(D2s, '/variableimportance/15Locs/D_values/D2s.csv')

D3s<-list()
D3s<-D3[[1]]
for (i in 2:100){
  D3s<-rbind(D3s, D3[[i]])
}
write.csv(D3s, '/variableimportance/15Locs/D_values/D3s.csv')

D4s<-list()
D4s<-D4[[1]]
for (i in 2:100){
  D4s<-rbind(D4s, D4[[i]])
}
write.csv(D4s, '/variableimportance/15Locs/D_values/D4s.csv')

setwd('/variableimportance/15Locs/optsets')
write.csv(optsets1, 'optsetsAll.csv')
write.csv(optsets2, 'optsetsImp.csv')
write.csv(optsets3, 'optsetsLCcont.csv')
write.csv(optsets4, 'optsetsLC75.csv')
