#' ggplo2 theme default for ohchibi plots
#'
#' Generalized theme for ggplot2. As universal as possible
#' @keywords ggplot2, theme
#' @export
#' @examples
#' theme_ohchibi()


theme_ohchibi <- function (size_axis_text = 20, size_axis_title = 30,
                           legend_proportion_size = 4,size_legend_text = 20,
                           size_lines_panel = 0.3,size_panel_border = 1,
                           x_hjust = 0.5, y_vjust = 0.5,font_family = "Arial") { 
  theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.y = element_line(colour =   "#D9D9D9",size=size_lines_panel),
        panel.grid.minor.y = element_line(colour = "#D9D9D9",size=size_lines_panel),
	panel.grid.major.x = element_blank(),
        panel.border = element_rect(fill=NA,color =  "#414141",size = size_panel_border),
        axis.ticks = element_line(colour = "black",size = 2.5),
        axis.text.x = element_text(family = font_family,face = "plain",size =size_axis_text,colour="#414141",hjust = x_hjust),
        axis.text.y = element_text(family = font_family,face="plain",size=size_axis_text,colour="#414141",vjust = y_vjust),
        axis.title.x = element_text(family = font_family,face="plain",size = size_axis_title,colour = "#414141"),
        axis.title.y = element_text(family = font_family,face="plain",size=size_axis_title,colour="#414141"),
        legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
        legend.title=element_blank(),legend.key = element_blank(), 
        legend.text = element_text(size=size_legend_text,
          family = font_family,face = "plain",colour = "#414141"),
        legend.position ="right"
        )
}

