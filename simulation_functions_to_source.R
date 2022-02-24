####CODE TO CREATE VIRTUAL SPECIES USING VIRTUALSPECIES R PACKAGE; AND CREATE LOCALITY DATASETS FROM SIMULATED SPECIES.

#####################
#####Define functions#####

packages = c('ENMeval', 'virtualspecies', 'rgdal', 'maptools', 'foreach','doParallel')
#####################
pkgs<- function(packages){
   for (K in 1:length(packages)){
    if (!require(packages[K], character.only=T)){
      install.packages(packages[K], repos='http://cran.mtu.edu/')
      require(packages[K], character.only=T)
    }}
}

##############
# depends on {raster} package
get.Data <- function(download, envdir, res, long, lats, cat.vars){
  ###Download worldlcim data tiles at the correct res for Madagascar
  if (download==TRUE){
    wC<-getData('worldclim', var='bio', res=res, lon=long, lat=lats)  #UNIQUE FOR MY CASE
    wC1<- crop(wC, cat.vars)      #CROP TO EXTENT OF SOIL LAYER
    env<- mask(wC1, cat.vars)     # MASK OUT PIXELS NOT IN SOIL LAYER
 }else {
    #setwd('C:/Users/pgalante/Documents/Projects/Madagascar/layers/test')
    #rasters<-list.files(path = envdir, full.names = T, pattern = ".asc$")
    env<- stack(list.files(path = envdir, full.names = T,  pattern = '.tif$'))
 }
 assign("env", env, envir = .GlobalEnv)
}
##################


####Performs Pearson's Correlation analysis, creates the list of variables that are Least correlated 
#### as well as the raster stack of least correlated variables.
Corr.Remove<-function(env, max.corr){
  COR = layerStats(env, 'pearson', na.rm=T)
  removed.vars<- removal.order(COR$`pearson correlation coefficient`, max.corr)
  envlist<-data.frame(names(env))
  corrl<-removed.vars[1]
  corr.list<-setdiff(envlist$names.env., corrl$VarRemoved)
  #corr.list<- removal.order(COR$`pearson correlation coefficient`, 0.01)
  assign("corr.list", corr.list, envir = .GlobalEnv)
  LCBio<- subset(env, corr.list)
  assign("LCBio", LCBio, envir = .GlobalEnv)
}


######create 10,000 background points from the entire study region (whole island) to 
######use for every iteration, for all tuning
#bg.coords<- randomPoints(Madbio[[1]], 10000)
#write.csv(bg.coords, 'C:/GIS/Madagas/Grant/Pete/Thesis/Thesis/Simulation/bg.coords.csv', row.names=FALSE)
get.bg<- function(backg, env){
  if (is.null(backg)){
    bg.coords<- randomPoints(env[[20]], 10000)
  }
  assign("bg.coords", bg.coords, envir = .GlobalEnv)
}

  #TOCHECK THIS OUT
  #traceback()
  #tryCatch()
  
  #####Set up parameters for simulated species. cat.response is a categorical variable generator that
  #####Swaps cat1 coefficient for categorical label of suit1, and the same for cat2/suit2, and gives zero values to all other 
  #####categorical labels. All parameter values are user specified.
  
create.niche<-function(cont.vars, means, SDs, cat.vars, cats, cat.vals, env, wd){
  contin.vars<-subset(env, cont.vars)
  categ.vars<-subset(env, cat.vars)
  usedvars<- stack(contin.vars, categ.vars)
  for (i in 1:nlayers(contin.vars)){
    if (i == 1){
      ffctns= sprintf("formatFunctions(%s= c(fun= 'dnorm', mean = %f, sd = %f)", names(usedvars)[i], means[i], SDs[i])
    }else{
      exp1<- sprintf("%s= c(fun= 'dnorm', mean = %f, sd = %f)", names(usedvars)[i], means[i], SDs[i])
      ffctns<-paste(ffctns, exp1, sep=', ')  
    }
    
    }
  for (i in 1:nlayers(categ.vars)){
    exp1<- sprintf("%s= c(fun= 'cat.response'", names(usedvars)[i+nlayers(contin.vars)])
    for (j in 1:length(cats)){
    exp1 <- sprintf("%s, cat%d=%d, suit%d=%f", exp1, j, cats[i], j, cat.vals[i])
    }
  ffctns<-paste(ffctns, exp1, sep=', ')
  }
  ffctns = paste0(ffctns,'))')
  sim.paramters = eval(parse(text=ffctns))
    ####Create the Simualted species
  my.sim.sp<- generateSpFromFun(usedvars,
                                parameters=sim.paramters,
                                plot= T)

  ####Extract and save suitability raster
  simniche<-my.sim.sp$suitab.raster
  writeRaster(simniche, paste(wd, 'simniche.tif', sep='/'), overwrite = T)
  assign("simniche", simniche, envir = .GlobalEnv)
  return(simniche)
}

  #####Create sample datasets that are proportional to the prediction value. 
  #####10 sets of 10 datasets of 15 locs
  #####Source the probsel function (select N random cells from raster with prob = cell value (suitability))
  #####value squared and convert to points):

create.locs<- function(nsets, simniche, locsdir, numlocs){  
 all.locs<-list()
    for (i in 1:nsets){
      locs <- list()
      pts <- probsel(simniche, numlocs)  #sample number of points
      locs <- pts@coords[,1:2]  #get just coords
    ###change output file name before running each time (e.g., modify the x)
      saveRDS(locs, paste(locsdir,'/', i,'.RDS',sep = ''))
      all.locs[[i]] <- locs
    }
  assign("all.locs", all.locs, envir = .GlobalEnv)
}
  
createniche<- function(packages, nsets, download, cont.vars, cats, means, SDs, cat.vars, 
                       cat.vals, backg, res, long, lats, envdir, locsdir, numlocs, max.corr, wd){
  ##nsets= # of simulated datasets required
  ##download refers to whether or not the user needs to download worldclim data 
  ##cont.vars= raster stack of continuous variables should be used in the virtual niche
  ##cat.vars= which (if any) categorical variables will be used (as is now, only 1 can be used)
  ##mean= concatenation mean responses of simulated species to each variable
  ##SDs= concatenation of SDs of responses of simulated species to each variable
  ##cat.vals= concatenation of responses simulated species should have to categorical items
  ##backg= background coordinated to use- should be predefined so standardize between sim. sp.
  ##res= worldclim resolution for download
  ##long= geographic region for tile download
  ##lats= see long  
  ##envdir= directory to environmental data to be used   RECOMMENDED
  ##locsdir= directory of where to save loc files (nsets)
 # pkgs(packages)
  get.Data(download= download, res = res, envdir = envdir, long = long, lats= lats, cat.vars= cat.vars)
  kk = get.bg(backg, env= get.Data(download= F, envdir= envdir, res= res, long= long, lats= lats))
  create.niche(cont.vars, means, SDs, cat.vars, cats, cat.vals, env,wd)
  #simniche<-raster('C:/Users/pgalante/Documents/Projects/PJG/simniche.asc')
  create.locs(nsets, simniche, locsdir, numlocs)
  Corr.Remove(env, max.corr)
}
  

  



