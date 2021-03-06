
#FUNCTION multi_rsf ########################################################################
# INPUT:
# dataset: dataframe
# features: list of char, number of features

multi_cox <- function(dataset, features){
  require(survival)
  multi_formula = formula(paste("Surv(time, os) ~ ", paste(features, collapse = " + ")))
  res.cox <- coxph(multi_formula, data =  dataset, x = TRUE)
  return(res.cox)
}



#############################################################################################