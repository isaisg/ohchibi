#' Plots results from adonis output
#'
#' Uses the aov table contained in out by adonis
#' @keywords adonis,permanova
#' @export
#' @examples
#' chibi.permanova()

#Assay for addendum for chibi.cap
chibi.permanova <- function(mypermanova = NULL,pval_thres = 0.05){
  pval_thres <- pval_thres
  df_aov <- mypermanova$aov.tab %>% as.data.frame
  colnames(df_aov)[6] <- "pvalue"
  df_aov$Term <- rownames(df_aov) %>% factor
  df_aov$Description <- rep("Term",nrow(df_aov))
  df_aov <- df_aov %>% subset(Term != "Total") %>%
    droplevels
  df_aov <- df_aov %>% subset(pvalue <= pval_thres)

  residual <- 1- sum(df_aov$R2)
  df_plot <- df_aov[,c(5:8)] %>% droplevels
  df_plot <- with(df_plot,order(R2)) %>% df_plot[.,]
  ord_ele <- df_plot$Term %>% as.character
  df_plot <- data.frame(R2 = residual,pvalue = 1,
                        Term = "Residual",Description = "Term") %>%
    rbind(df_plot,.)
  df_plot$Term <- factor(df_plot$Term,levels = c("Residual",ord_ele))
  rownames(df_plot) <- NULL
  df_plot$R2 <- df_plot$R2*100
  p_var <- ggplot(data = df_plot, aes(x = Description,y = R2, fill = Term)) +
    geom_bar(stat = "identity") + ylab(label = "Variance Explained (%)") +
    theme_ohchibi(size_axis_title.x = 0) + theme(aspect.ratio = 3) +
    scale_y_continuous(breaks=seq(0,100,10)) +
    coord_cartesian(ylim = c(0,100), expand = FALSE)
  return(p_var)
}
