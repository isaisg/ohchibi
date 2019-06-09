#' ggplo2 theme default for ohchibi plots
#'
#' Generalized theme for ggplot2. As universal as possible
#' @keywords ggplot2, theme
#' @export
#' @examples
#' theme_ohchibi()


theme_ohchibi <- function (size_axis_text.x = 20,size_axis_text.y = 20,
                           size_axis_title.x = 30,size_axis_title.y = 30,
                           angle_text.x = 0,angle_text.y = 0,
                           legend_proportion_size = 1,
                           size_title_text = 30,size_legend_text = 20,
                           size_lines_panel = 0.3,size_panel_border = 2,
                           x_hjust = 0.5, y_vjust = 0.5,font_family = "Arial",size_ticks = 2) { 
  theme(
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
	panel.grid.major.x = element_blank(),
        panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
        axis.line = element_blank(),
	axis.ticks = element_line(colour = "black",size = size_ticks),
        axis.text.x = element_text(family = font_family,face = "bold",size =size_axis_text.x,colour="black",hjust = x_hjust,angle = angle_text.x),
        axis.text.y = element_text(family = font_family,face="bold",size=size_axis_text.y,colour="black",vjust = y_vjust, angle = angle_text.y),
        axis.title.x = element_text(family = font_family,face="bold",size = size_axis_title.x,colour = "black"),
        axis.title.y = element_text(family = font_family,face="bold",size=size_axis_title.y,colour="black"),
        legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
        legend.title=element_text(size=size_title_text,
          family = font_family,face = "bold",colour = "black"),
        legend.key = element_blank(), 
        legend.text = element_text(size=size_legend_text,
          family = font_family,face = "bold",colour = "black"),
        legend.position ="right",
        )
}

