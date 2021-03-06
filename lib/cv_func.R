#FUNCTION cv_func.R ########################################################################
# INPUT:
# dataset: dataframe
# treated: "t0", "t1", "td"
# datasetType: "lungR", "lungC", "lungI", "lungRCI"




cv1 <- function(model1, multi_formula, dataset, B = 10, seed = 13){
  require(pec)
  require(survival)
  require(randomForestSRC)
  .env <- environment()
  
  #
  # compute the apparent estimate of the C-index at different time points
  #
  set.seed(seed)
  ApparrentCindex <- pec::cindex(list("Cox"=model1),
                                 formula= multi_formula,
                                 data=na.omit(dataset),
                                 eval.times=seq(1,35,1))
  #print(ApparrentCindex)
  #plot(ApparrentCindex)
  #
  # compute the bootstrap-crossvalidation estimate of
  # the C-index at different time points
  #
  set.seed(seed)
  bcvCindex <- pec::cindex(list("Cox"=model1),
                           formula=multi_formula,
                           data=na.omit(dataset),
                           splitMethod="bootcv",
                           B=B,
                           eval.times=seq(1,35,1))
  print(bcvCindex)
  plot(bcvCindex)
  #mean(na.omit(bcvCindex$BootCvCindex[[1]])) # 0.66 Radiomics
  
  #
  # compute the bootstrap-crossvalidation estimate of
  # the C-index at different time points
  #
  set.seed(seed)
  bcvCindex632 <- pec::cindex(list("Cox"=model1),
                              formula=multi_formula,
                              data=na.omit(dataset),
                              splitMethod="Boot632",
                              B=B,
                              eval.times=seq(1,35,1))
  #print(bcvCindex632)
  #plot(bcvCindex632)
  AppCindex.cox <- mean(na.omit(bcvCindex632$AppCindex$Cox)) # 0.70 radiomics on training #0.78 for RCI
  BootCvCindex.cox <- mean(na.omit(bcvCindex632$BootCvCindex$Cox)) # 0.65 Radiomics cross validate # 0.74 FOR RCI
  Boot632Cindex.cox<- mean(na.omit(bcvCindex632$Boot632Cindex$Cox)) # 0.67 Radiomics cross validate # 0.76 FOR RCI
  
  
  cindex.cox <- c(AppCindex.cox=AppCindex.cox, BootCvCindex.cox=BootCvCindex.cox, Boot632Cindex.cox=Boot632Cindex.cox)
  
  bcvCindex632 <- c(bcvCindex632, list(cindex.cox = cindex.cox ))
  return(bcvCindex632)
  
}
