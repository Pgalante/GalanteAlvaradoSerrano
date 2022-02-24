# script to assess the order of variable removal to avoid autocorrelation
# the 3 sequential criteria for removal used by the script are:
# 1) highest pairwise correlation
# 2) highest average correlation of a variable with all other variables
# 3) highest average correlation of the entirse set of variables
# Diego F. Alvarado-S., Dec 12, 2014

removal.order = function(matrix, threshold=0.75){
    #matrix: full matrix of average correlations between variables; it assumes variables names are listed as colnames and varnames
    #threshold: maximum absolute average correlation of the entire matrix allowed and of any single variable

    mat = abs(matrix)
    diag(mat) = rep(NA, ncol(mat))
    REMOV.order = data.frame(VarRemoved=rep(NA,ncol(mat)), MeanCor=rep(NA,ncol(mat)), MaxCor=rep(NA,ncol(mat)))
    do.break = as.logical(F)
    for (i in 1:ncol(matrix)){
        if (do.break){
            cat(paste('Minimum requested threshold of', threshold, 'reached\n'))
            break
        } else{ if(!do.break){
            maxPAIRcor = rep(NA, ncol(mat))
            for (r in 1:nrow(mat)){
                if (length(which(is.na(mat[r,]))) != ncol(mat)){
                    maxPAIRcor[r] = max(mat[r,], na.rm=T)
            }}
            Pair = mat[,which(maxPAIRcor == max(maxPAIRcor, na.rm=T))]
            maxMEANcor = rep(NA, ncol(Pair))
            for (c in 1:ncol(Pair)){
                maxMEANcor[c] = mean(Pair[,c], na.rm=T)
            }
            MostCorVar = colnames(Pair)[which(maxMEANcor == max(maxMEANcor, na.rm=T))]
            maxTOTALcor = rep(NA, length(MostCorVar))
            if (length(MostCorVar) > 1){
                if (i!=ncol(matrix)-1){
                    for (x in 1:length(MostCorVar)){
                       maxTOTALcor[x] = mean(mat[-which(rownames(mat)==MostCorVar[x]),-which(colnames(mat)==MostCorVar[x])], na.rm=T)
                    }
                    MostCorVar = MostCorVar[which.max(maxTOTALcor)]
                } else {if (i!=ncol(matrix)-1){
                    MostCorVar=sample(MostCorVar,1)
                }}
            }
            mat[which(rownames(mat)==MostCorVar),] = rep(NA, nrow(mat))
            mat[,which(colnames(mat)==MostCorVar)] = rep(NA, ncol(mat))
            REMOV.order$VarRemoved[i] = MostCorVar
            REMOV.order$MeanCor[i] = mean(mat,na.rm=T)
            REMOV.order$MaxCor[i] = max(mat,na.rm=T)
            if (i==ncol(matrix)-1){
                do.break = as.logical(T)
            } else {if (i!=ncol(matrix)-1){
            if (mean(mat,na.rm=T)<threshold & max(mat,na.rm=T)<threshold){
                do.break = as.logical(T)
        }}}}}}
    return(na.omit(REMOV.order))
}