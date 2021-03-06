#FUNCTION getStableCor ########################################################################
# INPUT:
# dataset: dataframe
# nFeature: int, number of features
getStableCor <- function(df1,df2, threshold = 0.7, method = "pearson"){
  require("stats")
  mat<-cor(df1, df2, method = method)
  mat_diag<-diag(mat)
  mat_filtered <- mat_diag[mat_diag>threshold]
  #pearsonlist<-sort(mat_diag,decreasing=TRUE)
  return(names(mat_filtered))
  
}
