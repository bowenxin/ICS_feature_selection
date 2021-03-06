#FUNCTION getBestCluster (based on cox)####################################
# INPUT:
# dataset: dataframe
# nFeature: int, number of features
# rankby: "p.value" or "hr" or "cindex"

getBestCluster <- function(dataset, nFeature = 5, maxK = 15, optK = NULL,rankby = "hr", coef.threshold = NULL){
  dataset <- na.omit(dataset)
  require(dplyr)
  require(ConsensusClusterPlus)
  # clustering
  dataset_cluster <- select(dataset, -os, -time)
  #dataset_cluster <- na.omit(dataset_cluster)
  dataset_cluster.scaled<-scale(dataset_cluster)
  res = ConsensusClusterPlus(dataset_cluster.scaled,maxK=maxK,reps=50,pItem=0.8,pFeature=1,clusterAlg="hc",distance="pearson",seed=1262118388.71279,plot=NULL)
  
  ######## select best K if optK is not given ######
  if (is.null(optK)){
    Kvec = 2:maxK
    area_cdf = rep(0,length(Kvec)) 
    names(area_cdf) = paste("K=",Kvec,sep="") # from 2 to maxK
    
    for(i in Kvec){
      M = res[[i]]$consensusMatrix
      Fn = ecdf(M[lower.tri(M)])
      area_cdf[i-1] <- integrate(Fn,0,1,subdivisions=2000)$value
    }#end for i
    
    delta <- c(area_cdf[1],diff(area_cdf)/area_cdf[2:length(area_cdf)])
    delta <- delta [-length(delta)]
    
    sd_cdf <- c()
    for(i in 1:(length(delta)-2)){
      sd_cdf[i] <- sd(delta[i:(i+2)])
    }
    optK <- length(sd_cdf[sd_cdf > 0.04]) + 1 + 1
  }
  
  print(optK)
  ######## save matrix and select best features ######
  features <- character()
  consensusClass <- res[[optK]]$consensusClass
  for (i in 1:optK){
    cluster_feature <- names(consensusClass[consensusClass == i])
    cluster_df <- select(na.omit(dataset), 1,2,cluster_feature)
    features <- c(features, getBestCox(cluster_df, nFeature = nFeature, rankby=rankby, coef.threshold = coef.threshold))
  }
  
  return(features)
}

