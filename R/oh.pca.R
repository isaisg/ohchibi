#' Performs Principal Component Analysis based on prcomp command
#'
#' Two fundamental structures needed. The abundance matrix (Tab) and metadata (Map)
#' @keywords pca
#' @export
#' @examples
#' oh.pca()

oh.pca<-function(Tab=Tab,Map=Map,retx = TRUE,center = FALSE,scale = FALSE,id_var="Sample_Id"){
  mpca <- prcomp(x = Tab,retx = retx ,center = center,scale. = scale)
  scores <- mpca$x %>% as.data.frame
  df_var <- (summary(mpca) %$% importance %>% as.data.frame)[2,]
  df_var <- df_var*100
  scores <- cbind(scores,rownames(scores))
  colnames(scores)[ncol(scores)] <- id_var
  scores <- merge(scores,Map,by = id_var)
  toret <- list(
    Map_pca = scores,
    variance_explained = df_var,
    prcomp = mpca
  )  
  return(toret)
}
