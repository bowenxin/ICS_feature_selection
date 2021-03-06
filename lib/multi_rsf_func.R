
#FUNCTION multi_rsf ########################################################################
# INPUT:
# dataset: dataframe
# features: list of char, number of features

multi_rsf <- function(dataset, features){
  require(randomForestSRC)
  multi_formula = formula(paste("Surv(time, os) ~ ", paste(features, collapse = " + ")))
  res.rsf <- rfsrc(multi_formula,
                   data=dataset,
                   ntree=150,
                   forest=TRUE)
  return(res.rsf)
}



#############################################################################################