
#FUNCTION getBestCox ########################################################################
# INPUT:
# dataset: dataframe
# nFeature: int, number of features
# rankby: "p.value" or "hr" or "cindex"
getBestCox <- function(dataset, nFeature=5, rankby="p.value", p.threshold = 0.05, coef.threshold = NULL){
  require("survival")
  
  
  # perform univariate analysis on all columns
  covariates <- colnames(dataset)
  if ("time" %in% colnames(dataset) & "os" %in% colnames(dataset)){
    covariates <- covariates[covariates != "time" & covariates != "os"]
  }else{
    stop("No time or os column found in the dataset.")
  }
  
  univ_formulas <- sapply(covariates,
                          function(x) as.formula(paste('Surv(time, os)~', x)))
  
  univ_models <- lapply( univ_formulas, function(x){coxph(x, data = dataset)})
  
  # extract result of univarate analysis
  
  # Extract data 
  univ_results <- lapply(univ_models,
                         function(x){ 
                           x <- summary(x)
                           p.value<-signif(x$wald["pvalue"], digits=2)
                           wald.test<-signif(x$wald["test"], digits=2)
                           beta<-signif(x$coef[1], digits=2);#coeficient beta
                           HR <-signif(x$coef[2], digits=2);#exp(beta)
                           HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                           HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                           HR <- paste0(HR, " (", 
                                        HR.confint.lower, "-", HR.confint.upper, ")")
                           cindex <- x$concordance['C']
                           res<-c(beta, HR, wald.test, p.value, cindex)
                           names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                         "p.value","cindex")
                           return(res)
                           #return(exp(cbind(coef(x),confint(x))))
                         })
  
  univ_results_validate <- list()
  for (i in 1:length(univ_results)){
     if (length(univ_results[[i]]) == 5){
     univ_results_validate <- c(univ_results_validate, univ_results[i])
    }
  } # in case log test not passed
  
  res <- t(as.data.frame(univ_results_validate, check.names = FALSE))
  res.extracted <- as.data.frame(res)
  
  # Feature Filtering & Ranking 
  res.filtered <- res.extracted[as.numeric(as.character(res.extracted$p.value) )<p.threshold, ]
  if (!is.null(coef.threshold)){
    res.filtered <- res.filtered[abs(as.numeric(as.character(res.filtered$beta) ))>coef.threshold, ]
  }

  res.filtered$beta <- as.numeric(as.character(res.filtered$beta))
  res.filtered$p.value <- as.numeric(as.character(res.filtered$p.value))
  res.filtered$cindex <- as.numeric(as.character(res.filtered$cindex))
  if (rankby=="p.value"){
    res.sorted <- res.filtered[order(res.filtered$p.value),]
  } else if (rankby=="hr"){
    res.sorted <- res.filtered[order(-abs(res.filtered$beta)),]
  } else if (rankby=="cindex"){
    res.sorted <- res.filtered[order(-(res.filtered$cindex)),]
  }
  
  print(res.sorted)
  cat("\n")
  
  # Select N features from ranking
  if (nrow(res.sorted) < nFeature){
    features <- rownames(res.sorted)
    warning("Features less than nFeature are found in the dataset")
  } else {
    features <- rownames(res.sorted[1:nFeature,])
  }
  return(features)
}

##End of Function#######################################################################