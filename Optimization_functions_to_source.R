
#### Function to extract optimal pred (now in logistic using ENMevaluate2), compare with simniche
compare.Preds<-function(mod.obj, opt.sets, simulation){
  pred<- mod.obj@predictions[[as.numeric(rownames(opt.sets))]]
  D <- nicheOverlap(pred, simulation, stat= "D", checkNegatives = FALSE)
  return(D)
}

#g<-compare.Preds(mod.obj= res, opt.sets=r1, simniche)

###Create variables for results
optsets1<- data.frame()
s1<-stack()
resA<-list()
D1<-list()

optsets2<- data.frame()
s2<-stack()
resB<-list()
D2<-list()

optsets3<- data.frame()
s3<-stack()
resC<-list()
D3<-list()

optsets4<- data.frame()
s4<-stack()
resD<-list()
D4<-list()

###optimize function
optimize <- function(res) {
  ###Remove any candidate model which has an AUC less than 0.51= models with no discrimination
  opt.auc <- res[res[,4] >= 0.5,]
  ###Remove any candidates which have no parameters
  no.param <- opt.auc[opt.auc[,13] > 1,]
  ###Remove any candidates where the AIC score was NA (too many parameters)
  noAICNA<- no.param[which(!is.na(no.param$AICc)),]
  ###Remove any models which have an OR of zero
  noOR0 <- noAICNA[noAICNA[,9] != 0,]
  ###Order the remaining list by lowest OR then highest AUC, sequentially
  ordered<-noOR0[with(noOR0, order(Mean.OR10, -Mean.AUC)), ]
  ###Grab the settings of that first model (the optimal model)
  ordered[1,]
}


####Find lambdas function
#####Code from JMK that converts maxent object lambdas file into a usable data.frame
#####This function takes dismo's maxent object and extracts lambdas file as DF
lambdasDF <- function(mx) {
  lambdas <- mx@lambdas[1:(length(mx@lambdas)-4)]
  ldas<- data.frame(var=sapply(lambdas, FUN=function(x) strsplit(x, ',')[[1]][1]),
                    coef=sapply(lambdas, FUN=function(x) as.numeric(strsplit(x, ',')[[1]][2])),
                    min=sapply(lambdas, FUN=function(x) as.numeric(strsplit(x, ',')[[1]][3])),
                    max=sapply(lambdas, FUN=function(x) as.numeric(strsplit(x, ',')[[1]][4])),
                    row.names=1:length(lambdas))
  
  #run function to extract lambdas as DF
  mm<-ldas
  #Take just first two columns
  mm<-mm[,1:2]
  #remove any rows that have a zero lambdas value (Second column)
  mm<-mm[apply(mm[2],1,function(z) !any(z==0)),]
  #remove any rows that have duplicate "var"s (hinges, quadratics)
  m1<-unique(sub("\\^\\S*", "", mm[,1]))
  m1<-unique(sub("\\`", "", m1))
  m1<-unique(sub("\\'", "", m1))
  m2<-unique(sub("\\=\\S*", "", m1))
  m3<-unique(sub("\\(", "", m2))
  #this object should be used for tuning in the next round
  return(m3)
}

###Define all functions

Tune.optimize.all<- function(locs, env, method, categoricals, fc, bg.coords, RMvalues, simulation){
  ###Run ENMevaluate using modified function ENMevaluate2 found in Var_importance.R in sourced_functions
  ###Use jackknife method, all variables, and few FCs, and restricted RMs
  res <- ENMEVAL(occ = locs, env = env, method=method, categoricals=cat.vars, 
                     fc = fc,bg.coords = bg.coords, RMvalues = RMvalues, parallel = T, numCores = 7)
  ###Grab the settings of that first model (the optimal model)
  r1<- optimize(res@results)
  assign("r1", r1, envir = .GlobalEnv)
  lambdas1<-lambdasDF(res@models[[as.numeric(rownames(r1))]])
  assign('lambdas1', lambdas1, envir= .GlobalEnv)
  D1[[i]]<<-compare.Preds(mod.obj = res,opt.sets = r1, simniche)
  s1<<- stack(res@predictions[[as.numeric(rownames(r1))]], s1)
  env2<-subset(env, lambdas1)
  assign('env2', env2, envir= .GlobalEnv)
  write(names(env2),paste(getwd(),'varsused/All.csv', sep='/'), append=T, ncolumns = length(names(env2)), sep=',')
}  
  
#Tune.optimize.all(locs, env, method='block', categoricals= cat.vars, bg.coords, fc= c("L"), RMvalues=seq(1,1.5,0.5), simniche)

#######
opt.vars.all<-function(lambdas1=lambdas1, r1, env=env2, cat.vars){
  ##Add the optimal settings from this model to optsets1 table
  optsets1 <<- rbind(optsets1, r1)
  ##Add results table to other results tables
  resA[[i]]<<- r1
  ###If the optimal model is created using one variable, then inform user (the next use of
  ###ENMevaluate2 will crash if parameters is only 1)
  if(length(lambdas1)<2){cat('FAIL- not enough variables included\n')}
  ###If variable "geotest" is included, make sure it is marked as categorical, if not then NULL
  cat(paste('IMP. Var.:', paste(names(env2),collapse=' '),'\n'))
  cat.true<-if(cat.vars %in% names(env2)) {cat.true=cat.vars} else (cat.true=NULL)
  assign('cat.true', cat.true, envir= .GlobalEnv)
}

#######  
Tune.optimize.Imp<-function(locs, method, categoricals, fc, bg.coords, RMvalues, lambdas1, r1, env2, preds){
  opt.vars.all(lambdas1, r1, env2, cat.vars)
  ######Retune same model with same variables used in model tuned allowing all variables
  res2 <- ENMEVAL(occ= locs, env= env2, method=method, categoricals=cat.true, fc= fc, bg.coords= bg.coords, RMvalues= RMvalues,
                       parallel = T, numCores = 7)
  ###Grab the settings of the optimal model
  r4<-optimize(res2@results)
  lambdas2<-lambdasDF(res2@models[[as.numeric(rownames(r4))]])
  ###Add the optimal settings from this model to the optsets2 table as above
  optsets2 <<- rbind(optsets2, r4)
  resB[[i]]<<- res2@results
  assign("r4", r4, envir = .GlobalEnv)
  lambdas2<-lambdasDF(res2@models[[as.numeric(rownames(r4))]])
  D2[[i]]<<-compare.Preds(mod.obj = res2,opt.sets = r4, simniche)
  s2<<- stack(res2@predictions[[as.numeric(rownames(r4))]], s2)
  write(lambdas2,paste(getwd(),'varsused/Imp.csv', sep='/'), append=T, ncolumns = length(lambdas2), sep=',')
} 

#Tune.optimize.Imp(locs, method='block', categoricals, fc, bg.coords, RMvalues, lambdas1, r1, env2, preds)

#######
opt.vars.Imp<-function(env, corr.list, lambdas1){
  ####Create new stack of the same number of least correlated vars 
  env3<- env[[c(corr.list[1:length(lambdas1)])]]
#which(names(env) %in% corr.list[1:length(lambdas1),][[1]])
#  env3<-subset(env, sel)
  cat(paste('Least Corr. Var.:', paste(names(env3),collapse=' '),'\n'))
  ###If variable "geotest" is included, make sure it is marked as categorical, if not then NULL
  cat.true2<-if('geotest' %in% names(env3)) {cat.true2='geotest'} else (cat.true2=NULL)
  assign('env3', env3, envir = .GlobalEnv)
  assign('cat.true2', cat.true2, envir = .GlobalEnv)
}

#opt.vars.Imp(env, corr.list, lambdas1)

####### Least Correlated Control
Tune.optimize.LCcont<-function(locs, method, categoricals, fc, bg.coords, RMvalues, corr.list, lambdas1, env){
  opt.vars.Imp(env, corr.list, lambdas1)
  ######Retune with same number of least correlated variables as used in opt.vars.Imp.
  res3 <- ENMEVAL(occ=locs, env= env3, method=method, categoricals=cat.true2, fc=fc, bg.coords=bg.coords, RMvalues=RMvalues,
                       parallel = T, numCores = 7)
  ###Run function 'optimize' that will get the optimal model
  r6<- optimize(res3@results)
  lambdas3<-lambdasDF(res3@models[[as.numeric(rownames(r6))]])
  optsets3<<- rbind(optsets3, r6)
  resC[[i]]<<- res3@results
  D3[[i]]<<-compare.Preds(mod.obj = res3,opt.sets = r6, simniche)
  s3<<- stack(res3@predictions[[as.numeric(rownames(r6))]], s3)
  write(lambdas3,paste(getwd(),'varsused/LCcont.csv',sep='/'), append=T, ncolumns = length(lambdas3), sep=',')
}  

opt.vars.LCc<-function(env, corr.list, lambdas1){
  ####Create new stack of the same number of least correlated vars 
  env3<- env[[c(corr.list[1:length(lambdas1)])]]
  #which(names(env) %in% corr.list[1:length(lambdas1),][[1]])
  #  env3<-subset(env, sel)
  cat(paste('Least Corr. Var.:', paste(names(env3),collapse=' '),'\n'))
  ###If variable "geotest" is included, make sure it is marked as categorical, if not then NULL
  cat.true2<-if('geotest' %in% names(env3)) {cat.true2='geotest'} else (cat.true2=NULL)
  assign('env3', env3, envir = .GlobalEnv)
  assign('cat.true2', cat.true2, envir = .GlobalEnv)
}
#Tune.optimize.LCcont(locs, method= 'block', categoricals, fc, bg.coords, RMvalues, corr.list, lambdas1, env)

####### Least correlated thresholded
Tune.optimize.LC<-function(locs, LCBio, method, categoricals, fc, bg.coords, RMvalues){
  #####Tune using variables with less than 0.65 correlation
  res4 <- ENMEVAL(occ = locs, env = LCBio, method=method, categoricals=cat.vars, fc=fc, bg.coords=bg.coords, RMvalues=RMvalues,
                       parallel = T, numCores = 7)
  r7<-optimize(res4@results)
  lambdas4<-lambdasDF(res4@models[[as.numeric(rownames(r7))]])
  ##Bind the optimal settings from this model
  optsets4<<- rbind(optsets4, r7)
  resD[[i]]<<- res4@results
  D4[[i]]<<-compare.Preds(mod.obj = res4,opt.sets = r7, simniche)
  s4<<- stack(res4@predictions[[as.numeric(rownames(r7))]], s4)
  write(lambdas4,paste(getwd(),'varsused/LCthresh.csv',sep='/'), append=T, ncolumns = length(lambdas4), sep=',')
}

#Tune.optimize.LC(locs, LCBio, method='block', categoricals= cat.vars, fc= fc,bg.coords= bg.coords, RMvalues= RMvalues)

#######
DO.OPTIMIZATION<-function(locs, env, method, categoricals, fc, bg.coords, RMvalues, cat.vars){
  Tune.optimize.all(locs = locs, env = env, method=method, categoricals= cat.vars, bg.coords = bg.coords, fc = fc, RMvalues = RMvalues, simulation= simniche)
  #opt.vars.all(lambdas1, r1, env, cat.vars)
  Tune.optimize.Imp(locs = locs, method = method, categoricals=cat.vars, fc = fc, bg.coords = bg.coords, RMvalues = RMvalues, lambdas1 = lambdas1, r1 = r1, env2 = env2, preds = preds)
  #opt.vars.Imp(env, corr.list, lambdas1)
  Tune.optimize.LCcont(locs = locs, method = method, categoricals=cat.vars, fc = fc, bg.coords = bg.coords, RMvalues = RMvalues, corr.list = corr.list, lambdas1 = lambdas1, env = env)
  Tune.optimize.LC(locs = locs, LCBio = LCBio, method = method, categoricals= cat.vars, fc= fc, bg.coords= bg.coords, RMvalues= RMvalues)
}
#######



#######SAVING EVERYTHING PROPERLY#####

# ####FOR ALL VARS (res1)
# write.csv(optsets1, 'Results/all/1/optsets1.csv')
# write.csv(resA,'Results/all/1/results.csv')
# setwd('Results/all/1')
# unstacks1<- unstack(s1)
# outputnames <- paste(seq_along(unstacks1), ".asc",sep="")
# for(i in seq_along(unstacks1)){writeRaster(unstacks1[[i]], file=outputnames[i])}
# 
# ####FOR RETUNING WITH 'IMPORTANT' VARS (res2)
# write.csv(optsets2, 'Results/imp/1/optsets2.csv')
# write.csv(resB, 'Results/imp/1/results.csv')
# setwd('Results/imp/1/')
# unstacks2<- unstack(s2)
# outputnames <- paste(seq_along(unstacks2), ".asc",sep="")
# for(i in seq_along(unstacks2)){writeRaster(unstacks2[[i]], file=outputnames[i])}
# 
# ######FOR LEAST CORR VARS, NUMBER = SAME AS IMPORTANT (res3)
# write.csv(optsets3, 'Results/least/1/optsets3.csv')
# write.csv(resC, 'Results/least/1/results.csv')
# setwd('Results/least/1/')
# unstacks3<- unstack(s3)
# outputnames <- paste(seq_along(unstacks3), ".asc",sep="")
# for(i in seq_along(unstacks3)){writeRaster(unstacks3[[i]], file=outputnames[i])}
# 
# ####FOR Least correlated, LESS THAN 0.65 (res4)
# write.csv(optsets4, 'Results/LC.75/1/optsets2.csv')
# write.csv(resD, 'Results/LC.75/1/results.csv')
# setwd('Results/LC.75/1/')
# unstacks4<- unstack(s4)
# outputnames <- paste(seq_along(unstacks4), ".asc",sep="")
# for(i in seq_along(unstacks4)){writeRaster(unstacks4[[i]], file=outputnames[i])}
