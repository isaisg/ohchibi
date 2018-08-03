#' Wrapper that takes a gff dataframe, a dataframe with gene_ids and hmm ids (oh.profiler.pl output)and returns
#' two structures. One is a modified gff with the hotspots localized and another, a df summarizing the
#' characteristics of the hotspots identified.
#'
#' 
#' @keywords hotspot,indices,wrapper,gff
#' @export
#' @examples
#' oh.wrapper.hotspot.profiler()



oh.wrapper.hotspot.profiler <- function(df_gff = NULL, df_profiler = NULL, threshold = 10){
  mres <- oh.hotspot.finder(df_gff = df_gff,gene_oids = df_profiler$gene_oid,thres = threshold)
  mres <- merge(mres,df_profiler,by = "gene_oid",all = TRUE)
  mhs <- mres$hotspot  %>% unique  %>% na.omit  %>% as.vector
  Res <- NULL
  if(length(mhs) != 0 ){
    for(region in mhs){
      mres_sub <- mres  %>% subset(hotspot == region) %>% droplevels
      df_freq <- mres_sub$HMM_hit %>% table %>% as.data.frame
      colnames(df_freq)[1] <- "HMM"
      total_hits <- df_freq$Freq %>% sum
      total_motifs <- df_freq$HMM %>% unique %>% as.character
      rich <- paste(total_motifs,collapse = "|")
      gids <- paste(mres_sub$gene_oid,collapse = "|")
      glocus <- paste(mres_sub$locus_tag,collapse = "|")
      Res <- data.frame(Hotspot = region,total_hits = total_hits, total_different_HMMs = length(total_motifs),
                        id_different_HMMS = rich,id_all_HMMS = paste(mres_sub$HMM_hit,collapse = "|"),gene_oids = gids,loci_tags = glocus)%>% 
        rbind(Res,.)
    }
    Res <- with(Res,order(-total_different_HMMs)) %>%  Res[.,]
  }else{
    Res <- data.frame(Hotspot = 0,total_hits = 0, total_different_HMMs = 0,
                      id_different_HMMS = NA,gene_oids = NA,loci_tags = NA,taxon_oid = NA)%>% 
      rbind(Res,.)
  }
  return(list(gff_hotspot = mres, df_hotspots = Res))
}

