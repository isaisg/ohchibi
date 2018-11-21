#' Plots results from aov output
#'
#' Uses the aov table contained in out by aov
#' @keywords chibi,anova,plot
#' @export
#' @examples
#' chibi.anova()

chibi.anova <- function(myanova = NULL,pval_thres = 0.05,legend_proportion_size =2,
y_vjust=1,size_axis_text=20,size_axis_title=30,size_legend_text=20,size_title_text = 30,
size_ticks_x = 2.5, size_ticks_y =2.5, font_family = "Arial",aspect.ratio =3,size_panel_border = 0.1){
  pval_thres <- pval_thres
  df_aov <- myanova %>% anova %>% as.data.frame
  afss <- df_aov$"Sum Sq"
  df_aov$VarExp <- (afss/sum(afss))*100
  colnames(df_aov)[5] <- "pvalue"
  df_aov$Term <- rownames(df_aov) %>% factor
  df_aov$Description <- rep("Term",nrow(df_aov))

  df_aov <- df_aov %>% subset(pvalue <= pval_thres)
  residual <- 100- sum(df_aov$VarExp)
  df_plot <- df_aov[,c(5:8)] %>% droplevels
  df_plot <- with(df_plot,order(VarExp)) %>% df_plot[.,]
  ord_ele <- df_plot$Term %>% as.character
  df_plot <- data.frame(pvalue = 1,VarExp = residual,
                        Term = "Residual",Description = "Term") %>%
    rbind(df_plot,.)
  df_plot$Term <- factor(df_plot$Term,levels = c("Residual",ord_ele))
  rownames(df_plot) <- NULL

  p_var <- ggplot(data = df_plot, aes(x = Description,y = VarExp, fill = Term)) +
    geom_bar(stat = "identity") + ylab(label = "Variance Explained (%)") +
     theme(aspect.ratio = aspect.ratio) +
    scale_y_continuous(breaks=seq(0,100,10)) +
    coord_cartesian(ylim = c(0,100), expand = FALSE) +
    theme(
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", size = size_panel_border, fill = NA),
          axis.ticks.y =element_line(colour = "black", size = size_ticks_y),
          axis.ticks.x =element_line(colour = "black", size = size_ticks_x),
          axis.text.x =element_blank(),
          axis.text.y = element_text(family = font_family,face="plain",size=size_axis_text,colour="#414141",vjust = y_vjust),
          axis.title.x = element_text(family = font_family,face="plain",size = size_axis_title,colour = "#414141"),
          axis.title.y = element_text(family = font_family,face="plain",size=size_axis_title,colour="#414141"),
          legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
          legend.title=element_text(size=size_title_text,
          family = font_family,face = "plain",colour = "#414141"),
          legend.key = element_blank(),
          legend.text = element_text(size=size_legend_text,family = font_family,face = "plain",colour = "#414141"),
          legend.position ="right"
          )
  return(p_var)
}
