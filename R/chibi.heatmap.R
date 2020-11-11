#' Plot a  heatmap
#'
#' Automatically clusters, and plot
#' @keywords chibi,heatmap,ggplot
#' @export
#' @examples
#' chibi.heatmap()
chibi.heatmap <- function(Tab = NULL,dist_method_rows = "pearson",dist_method_cols = "euclidean",
                          hclust_method_rows = "complete",hclust_method_cols = "ward.D2",
                          k_rows = 8,k_cols = 4,palette_heatmap = "pals::kovesi.diverging_bwr_55_98_c37",
                          range_fill_heatmap = 1,size_strip_text_row = 10,size_strip_text_col = 0,
                          size_axis_text_col = 10,size_axis_text_row = 0,axis_ticks_row = F,
                          size_legend_text = 10, size_legend_title = 12,
                          size_dendrogram = 0.3,panel_border_heatmap = 0.3,panel_spacing = 0,
                          font_family = "Helvetica",font_face = "plain",legend_position = "right",
                          egg_heights = c(0.5,1),egg_widths = c(0.2,1)){
  if(dist_method_rows == "pearson" | dist_method_rows == "spearman"){
    dist_rows <- as.dist(1-cor(Tab %>% t,method = dist_method_rows))
    
  }else{
    dist_rows <- dist(Tab,method = dist_method_rows)
  }
  mclust_rows <- hclust(d = dist_rows,method = hclust_method_rows) 
  #mclust_genes %>% plot
  #rect.hclust(tree = mclust_genes,k = k_rows)
  order_rows <- mclust_rows$order %>% mclust_rows$labels[.]
  
  df_clust_rows <- mclust_rows %>% cutree(k = k_rows) %>%
    data.frame(IdRows = names(.), ClusterRows = paste0("CR",.),row.names = NULL)
  
  df_clust_rows <- df_clust_rows[,-1]
  
  #Cluster cols
  if(dist_method_cols == "pearson" | dist_method_cols == "spearman"){
    dist_cols <- as.dist(1-cor(Tab ,method = dist_method_cols))
    mclust_cols <- hclust(d = dist_cols,method = hclust_method_cols)
  }else{
    mclust_cols <- dist_genes <- dist(Tab %>% t,method = dist_method_cols) %>%
      hclust(method = hclust_method_cols)
  }

  order_cols <- mclust_cols$order %>% mclust_cols$labels[.]
  
  #mclust_genotype %>% plot
  #rect.hclust(tree = mclust_genotype,k = 4)
  df_clust_cols <- mclust_cols %>% cutree(k = k_cols) %>%
    data.frame(IdCols = names(.), ClusterCols = paste0("CC",.),row.names = NULL)
  df_clust_cols <- df_clust_cols[,-1]
  
  
  melted_sub <- Tab %>% melt
  colnames(melted_sub) <- c("IdRows","IdCols","value")
  
  melted_sub <- merge(melted_sub,df_clust_rows, by = "IdRows")
  melted_sub <- merge(melted_sub,df_clust_cols, by = "IdCols")
  
  melted_sub$IdRows <- melted_sub$IdRows %>% 
    factor(levels = order_rows)
  melted_sub$IdCols <- melted_sub$IdCols %>%
    factor(levels = order_cols)
  
  order_groups_rows <- with(melted_sub,order(IdRows)) %>%
    melted_sub$ClusterRows[.] %>% as.character %>%
    unique
  
  melted_sub$ClusterRows <- melted_sub$ClusterRows %>%
    factor(levels = order_groups_rows %>% rev)
  
  
  order_groups_cols <- with(melted_sub,order(IdCols)) %>%
    melted_sub$ClusterCols[.] %>% as.character %>%
    unique
  
  melted_sub$ClusterCols <- melted_sub$ClusterCols %>%
    factor(levels = order_groups_cols)
  
  #Create plot of distribution
  mvals <- melted_sub$value %>% sort
  mtemp <- data.frame(Index = 1:length(mvals),value = mvals)
  p_distri <- ggplot(mtemp,aes(Index,mvals)) +
    geom_point()  + theme_minimal()
  

  p <- ggplot(data = melted_sub,mapping = aes(IdCols,IdRows)) + 
    geom_raster(aes(fill = value),color = "#00000000") + 
    #geom_tile(aes(color = Significance),fill = '#00000000',
    #          size = 1,width = 0.95,height = 0.95) + 
    facet_grid(ClusterRows~ClusterCols,scales = "free",space = "free") +
    theme_ohchibi(font_family = font_family
                  ) + 
    scale_x_discrete(expand = c(0,0)) +
    scale_fill_paletteer_c(palette_heatmap,
                           limits = c(-range_fill_heatmap,range_fill_heatmap),oob = squish,name = "z-score") +
    scale_color_manual(values = "black",na.value = "#00000000")
   if(axis_ticks_row == FALSE){
    p_heatmap <-  p +     theme(
       axis.text.y = element_blank(),
       axis.ticks.y = element_blank(),
       axis.title.x = element_blank(),
       axis.text.x = element_text(family = font_family,face = font_face,size =size_axis_text_col ,angle = 45,vjust = 1,hjust = 1),
       strip.background.y = element_blank(),
       strip.text.y = element_text(size = size_strip_text_row,family = font_family,face = font_face,angle = 0),
       strip.background.x = element_blank(),
       strip.text.x = element_text(size = size_strip_text_col,family = font_family,face = font_face),
       panel.border = element_rect(size = panel_border_heatmap),
       axis.title.y = element_blank(),
       panel.spacing.y  = unit(panel_spacing, "lines"),
       panel.spacing.x  = unit(panel_spacing, "lines"),
       legend.position = legend_position,
       legend.text = element_text(family = font_family,face = font_face,size = size_legend_text),
       legend.title = element_text(family = font_family,face = font_face,size = size_legend_title)
       
     )  
   }else{
     p_heatmap <-  p +     theme(
       axis.text.y = element_text(family = font_family,face = font_face,size = size_axis_text_row),
       axis.title.x = element_blank(),
       axis.text.x = element_text(family = font_family,face = font_face,size =size_axis_text_col ,angle = 45,vjust = 1,hjust = 1),
       strip.background.y = element_blank(),
       strip.text.y = element_text(size = size_strip_text_row,family = font_family,face = font_face,angle = 0),
       strip.background.x = element_blank(),
       strip.text.x = element_text(size = size_strip_text_col,family = font_family,face = font_face),
       panel.border = element_rect(size = panel_border_heatmap),
       axis.title.y = element_blank(),
       panel.spacing.y  = unit(panel_spacing, "lines"),
       panel.spacing.x  = unit(panel_spacing, "lines"),
       legend.position = legend_position ,
       legend.text = element_text(family = font_family,face = font_face,size = size_legend_text),
       legend.title = element_text(family = font_family,face = font_face,size = size_legend_title)
       
       
     )  
   }

  
  #Create composite figure
  ### Tree part ###
  tree_cols <- mclust_cols %>% as.phylo
  p_tree_cols  <- ggtree(tree_cols,ladderize = F,size = size_dendrogram) + 
    #The expand parth is fundamental to make the tree fit the composition with the other plots
    scale_y_continuous(expand = c(0.01,0.001)) +
    coord_flip()  + scale_x_reverse()
  
  tree_rows <- mclust_rows %>% as.phylo
  p_tree_rows <- ggtree(tree_rows,ladderize = F,size = size_dendrogram) + 
    #The expand parth is fundamental to make the tree fit the composition with the other plots
    scale_y_continuous(expand = c(0.001,0.001))
  
  p_blank <- ggplot() +theme_void()
  
  ###### Arrange figures ##########
  composition <- egg::ggarrange(p_blank,p_tree_cols,
                                p_tree_rows,p_heatmap,
                                ncol = 2,nrow = 2,byrow = T,
                                heights = egg_heights,
                                widths = egg_widths,padding = unit(20,"line"),draw = F)
  #Create list to return
  mres <- list(
    mclust_rows = mclust_rows,
    mclust_cols = mclust_cols,
    melted = melted_sub,
    heatmap = composition,
    p_dist = p_distri,
    p_heatmap = p_heatmap,
    p_tree_rows = p_tree_rows,
    p_treee_cols = p_tree_cols
  )
  return(mres)
}

