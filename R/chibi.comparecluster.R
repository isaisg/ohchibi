#' Perform GO enrichment comparing clusters
#'
#' Automatically perform the comparison at 3 levels
#' @keywords chibi,GO,ggplot
#' @export
#' @examples
#' chibi.comparecluster()
chibi.comparecluster <- function(melted = NULL,paleta = "viridis::plasma",
                                 showCategory = 20,cutoff_redundancy = 0.7){
  mclusters <- melted$ClusterRows  %>% levels
  mlist <- list()
  for(mclust in mclusters){
    mgs <- melted %>% subset(ClusterRows == mclust) %$% IdRows %>% as.character  %>%
      unique
    mlist[[mclust]] <- mgs
  }
  
  ## Gene ontology analysis
  cg <- compareCluster(geneCluster=mlist,
                       fun="enrichGO",
                       keyType       = "TAIR",
                       OrgDb         = org.At.tair.db,
                       ont           = "BP",
                       pAdjustMethod = "BH",
                       pvalueCutoff  = 0.05,
                       qvalueCutoff  = 0.1)
  df_cg <- cg %>% as.data.frame
  
  #Three representations
  p_all <- clusterProfiler::dotplot(cg, showCategory=showCategory, includeAll=TRUE)+
    scale_color_paletteer_c(paleta,na.value = "#BFBFBF") 
  
  ### Shot high level enrichment ###
  df_high <- gofilter(x = cg,level = 4) 
  p_high <- clusterProfiler::dotplot(df_high, showCategory=showCategory, includeAll=TRUE)+
    scale_color_paletteer_c( paleta,na.value = "#BFBFBF") 

  
  ### Remove redundancy ####
  bp2 <- clusterProfiler::simplify(cg, cutoff=cutoff_redundancy, by="p.adjust", select_fun=min)
  p_red <- clusterProfiler::dotplot(bp2, showCategory=showCategory, includeAll=TRUE)+
    scale_color_paletteer_c(paleta,na.value = "#BFBFBF") 
  mtemp <- list(
    mlist = mlist,
    cg = cg,
    df_cg = df_cg,
    df_high = df_high,
    p_all = p_all,
    p_red = p_red,
    p_high = p_high
  )
  return(mtemp)
  
}

