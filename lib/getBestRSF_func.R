#FUNCTION getBestRSF ########################################################################
# INPUT:
# dataset: dataframe
# nFeature: int, number of features

getBestRSF <- function(dataset, nFeature, seed = 1000, ntree = 2000){
  # Build Model
  require("randomForestSRC")
  
  set.seed(seed)
  train <- sample(1:nrow(dataset), round(nrow(dataset) * 1), replace = FALSE) # train split ratio = 1
  traindata <- dataset[train, ]
  testdata <- dataset[-train, ]
  fit <- rfsrc(Surv(time,os)~ . ,
               data = traindata, 
               ntree = ntree, 
               mtry = round(sqrt(ncol(dataset))), 
               nsplit =  round(sqrt(ncol(dataset))), 
               importance = TRUE
  )
  # Feature selection
  features = names(sort(fit$importance, decreasing = TRUE)[1:nFeature])
  return(features)
  
}