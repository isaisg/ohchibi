#' Takes a gff df read by oh.read.gff and a list of gene_oids and identify hotspots where the
#' passed gene_oids are in physical proximity. Returns a dataframe with the identified hotspots.
#'
#' 
#' @keywords hotspot,indices,wrapper,gff
#' @export
#' @examples
#' oh.hotspot.finderx()


oh.hotspot.finder <- function(df_gff = NULL,gene_oids = NULL,thres = 10){
  gene_oids <- gene_oids %>% as.character
  df_gff$Phenotype <- rep("No",nrow(df_gff))
  df_gff$Phenotype[df_gff$gene_oid %in% gene_oids %>% which] <- "Yes"
  #Determine number of contigs
  contigs <- df_gff$seqid %>% unique %>% as.character
  #Work over each contig
  num_h <- 0
  res <- NULL
  for(contig in contigs){
    df_sub <- df_gff %>% subset(seqid == contig) %>% droplevels
    #Create indices
    df_sub$index <- seq(1,nrow(df_sub))
    df_sub_p <- df_sub %>% subset(Phenotype == "Yes") %>% droplevels
    #Evaluate if not positives in given contig
    if(nrow(df_sub_p) == 0){
      df_sub$hotspot <- rep(NA,nrow(df_sub))
      res <- rbind(res,df_sub)
    }else{
      #Perform computation of hotspots based on indices separation
      t_res <- oh.hotspot.byindex(thres = thres,indices = df_sub_p$index,
                                  
                                  num_hotspot = num_h)
      tnum_h <- t_res$hotspot %>% gsub(pattern = "Hotspot",replacement = "") %>%
        as.numeric %>% max
      #If there was no overlap found dont change the num_h
      #Num_h holds the total hits per genome
      if(!is.na(tnum_h)){
        num_h <- tnum_h
      }
      df_sub <- merge(df_sub,t_res,by = "index",all = T)
      res <- rbind(res,df_sub)
    }
  }
  return(res)
  res$hotspot <- res$hotspot 
}

