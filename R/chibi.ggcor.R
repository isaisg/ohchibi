#' Plot a correlation matrix as a heatmap
#'
#' Automatically clusters, calculate p.values, adjust p.values and plots
#' @keywords chibi,correlation,ggplot
#' @export
#' @examples
#' chibi.ggcor()



chibi.ggcor <- function(Tab = NULL,hclust_method = "ward.D",cor.method = "pearson",
                        display.values = TRUE,display.significance = TRUE,
                        p.adjust.method = "holm",p.adj.thres = 0.1){
  mclust_ed <- as.dist(1-cor(Tab,method = cor.method)) %>% hclust(method = hclust_method) 
  order_r <- mclust_ed$order %>% mclust_ed$labels[.]
  res_rcorr <- Hmisc::rcorr(Tab_ed,type = cor.method)
  melted_r <- res_rcorr$r %>% melt_dist
  melted_p <- res_rcorr$P %>% melt_dist
  melted_p$p.adj <- melted_p$dist %>% p.adjust(method = p.adjust.method)
  melted_r$UId <- paste(melted_r$iso1,melted_r$iso2,sep = "|")
  melted_p$UId <- paste(melted_p$iso1,melted_p$iso2, sep = "|")    
  merged <- merge(melted_r,melted_p[,-c(1,2)], by = "UId")
  colnames(merged) <- c("UId","Var1","Var2","r","p.value","p.adj")
  merged <- merged[,-1]
  merged_inv <- data.frame(Var1 = merged$Var2, Var2 = merged$Var1, r = merged$r,
             p.value = merged$p.value, p.adj = merged$p.adj)
  merged <- rbind(merged,merged_inv)
  uid <- c(merged$Var1,merged$Var2) %>% unique
  merged <- data.frame(Var1 = uid,Var2 = uid, r = 1,p.value = 0,p.adj = 0 ) %>% 
    rbind(merged,.)
  merged$Var1 <- merged$Var1 %>% factor(levels = order_r)
  merged$Var2 <- merged$Var2 %>% factor(levels = order_r)
  merged$Significance <- "NoSignificant"
  merged$Significance[which(merged$p.adj < p.adj.thres)] <- "Significant"
  p <- ggplot(data = merged,aes(Var1,Var2)) +
    geom_raster(aes(fill = r)) + 
    scale_fill_paletteer_c(package = "pals",palette = "kovesi.diverging_bwr_55_98_c37") +
    scale_color_manual(values = c("#00000000","black"))+
    theme_ohchibi(legend_proportion_size = NA,size_panel_border = 2) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1)
    )
  if(display.values == TRUE){
    p <- p + geom_text(aes(label = round(r,2))) 
  }
  if(display.significance == TRUE){
    p <- p + geom_tile(aes(color = Significance),fill = '#00000000', size = 1,width = 0.85,height = 0.85) 
      
  }
   Res <- list(p = p,df_cor = merged,mclust = mclust_ed)
   return(Res)
}

