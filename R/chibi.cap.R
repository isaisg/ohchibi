#' Plots results from oh.cap
#'
#' Uses the structure return from oh.cap to automatically plot a scatterplot colored by variables from the Map structured used to compute oh.cap
#' @keywords cap,chibi,plot
#' @export
#' @examples
#' chibi.cap()


chibi.cap<-function(list_ohpco=NULL,col_val=NULL,shape_val=NULL,comp_a=NULL,comp_b=NULL,mypch=21,
        size=15,alpha=1,stroke=1,col_shape_background="white",alpha_shape_background=0,
        lines_zero = TRUE,size_axis_line = 0.5,type_axis_line = "longdash",size_ticks = 0.5,
        ratio_size_shape_background=1.3,y_vjust=0.5,x_hjust=0.5,size_axis_text=12,size_axis_title=13,font_face = "plain",
        size_legend_text=12,size_title_text = 13,legend_proportion_size=1,size_lines_panel = 0,size_panel_border = 1,font_family = "Helvetica"){
  mix.colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928")
  percvar<-list_ohpco$variance_explained_axis
  Map_cap<-list_ohpco$Map_cap
  vartot<-list_ohpco$total_var
  pval<-list_ohpco$pval_model
  if(lines_zero == FALSE) {
    if(is.null(shape_val)){
    p <- ggplot(data = Map_cap,aes_string(x = comp_a,y = comp_b))+
      geom_point(size = size,alpha=alpha,pch=mypch,colour="black",stroke = stroke,aes_string(fill = col_val))+
      geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+
      xlab(label = paste(comp_a,"(",percvar[which(names(percvar)==comp_a)],"%)",sep="")) +
      ylab(label = paste(comp_b,"(",percvar[which(names(percvar)==comp_b)],"%)",sep="")) +
      guides(fill = guide_legend(override.aes = list(size=12))) +
      theme(axis.line = element_blank(),
            panel.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
            panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
            panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
            axis.ticks = element_line(colour = "black",size = size_ticks),
            axis.text.x = element_text(family = font_family,face = font_face,size =size_axis_text,colour="black",hjust = x_hjust),
            axis.text.y = element_text(family = font_family,face=font_face,size=size_axis_text,colour="black",vjust = y_vjust),
            axis.title.x = element_text(family = font_family,face=font_face,size = size_axis_title,colour = "black"),
            axis.title.y = element_text(family = font_family,face=font_face,size=size_axis_title,colour="black"),
            legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
            legend.title=element_text(size=size_title_text,
          family = font_family,face = font_face,colour = "black"),
            legend.key = element_blank(),
            legend.text = element_text(size=size_legend_text,
                                       family = font_family,face = font_face,colour = "black"),
            legend.position ="right")
    p <- p + scale_fill_manual(values = mix.colors)
    #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
    return(p)
    }else{
      p <- ggplot(data = Map_cap,aes_string(x = comp_a,y = comp_b,shape = shape_val))+
        geom_point(size = size,alpha=alpha,aes_string(colour = col_val))+
        geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+
        xlab(label = paste(comp_a,"(",percvar[which(names(percvar)==comp_a)],"%)",sep="")) +
        ylab(label = paste(comp_b,"(",percvar[which(names(percvar)==comp_b)],"%)",sep="")) +
        guides(fill = guide_legend(override.aes = list(size=12))) +
        theme(axis.line = element_blank(),
              panel.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
              panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
              panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
              axis.ticks = element_line(colour = "black",size = size_ticks),
              axis.text.x = element_text(family = font_family,face = font_face,size =size_axis_text,colour="black",hjust = x_hjust),
              axis.text.y = element_text(family = font_family,face=font_face,size=size_axis_text,colour="black",vjust = y_vjust),
              axis.title.x = element_text(family = font_family,face=font_face,size = size_axis_title,colour = "black"),
              axis.title.y = element_text(family = font_family,face=font_face,size=size_axis_title,colour="black"),
              legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
              legend.title=element_text(size=size_title_text,
          family = font_family,face = font_face,colour = "black"),
              legend.key = element_blank(),
              legend.text = element_text(size=size_legend_text,
                                         family = font_family,face = font_face,colour = "black"),
              legend.position ="right")
      #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
      p <- p + scale_color_manual(values = mix.colors)
      return(p)
    }
	}else{
  if(is.null(shape_val)){
  p <- ggplot(data = Map_cap,aes_string(x = comp_a,y = comp_b))+
    geom_vline(xintercept = 0,size = size_axis_line,color = "#D9D9D9",linetype = type_axis_line) +
    geom_hline(yintercept = 0,size = size_axis_line,color = "#D9D9D9",linetype = type_axis_line) +
    geom_point(size = size,alpha=alpha,pch=mypch,colour="black",stroke = stroke,aes_string(fill = col_val))+
    geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+
    xlab(label = paste(comp_a,"(",percvar[which(names(percvar)==comp_a)],"%)",sep="")) +
    ylab(label = paste(comp_b,"(",percvar[which(names(percvar)==comp_b)],"%)",sep="")) +
    guides(fill = guide_legend(override.aes = list(size=12))) +
    theme(axis.line = element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
          panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
          panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
          axis.ticks = element_line(colour = "black",size = size_ticks),
          axis.text.x = element_text(family = font_family,face = font_face,size =size_axis_text,colour="black",hjust = x_hjust),
          axis.text.y = element_text(family = font_family,face=font_face,size=size_axis_text,colour="black",vjust = y_vjust),
          axis.title.x = element_text(family = font_family,face=font_face,size = size_axis_title,colour = "black"),
          axis.title.y = element_text(family = font_family,face=font_face,size=size_axis_title,colour="black"),
          legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
          legend.title=element_text(size=size_title_text,
        family = font_family,face = font_face,colour = "black"),
          legend.key = element_blank(),
          legend.text = element_text(size=size_legend_text,
                                     family = font_family,face = font_face,colour = "black"),
          legend.position ="right")
  p <- p + scale_fill_manual(values = mix.colors)

  #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
  return(p)
  }else{
    p <- ggplot(data = Map_cap,aes_string(x = comp_a,y = comp_b,shape = shape_val))+
      geom_vline(xintercept = 0,size = size_axis_line,color = "#D9D9D9",linetype = type_axis_line) +
      geom_hline(yintercept = 0,size = size_axis_line,color = "#D9D9D9",linetype = type_axis_line) +
      geom_point(size = size,alpha=alpha,aes_string(colour = col_val))+
      geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+
      xlab(label = paste(comp_a,"(",percvar[which(names(percvar)==comp_a)],"%)",sep="")) +
      ylab(label = paste(comp_b,"(",percvar[which(names(percvar)==comp_b)],"%)",sep="")) +
      guides(fill = guide_legend(override.aes = list(size=12))) +
      theme(axis.line = element_blank(),
            panel.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
            panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
            panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
            axis.ticks = element_line(colour = "black",size = size_ticks),
            axis.text.x = element_text(family = font_family,face = font_face,size =size_axis_text,colour="black",hjust = x_hjust),
            axis.text.y = element_text(family = font_family,face=font_face,size=size_axis_text,colour="black",vjust = y_vjust),
            axis.title.x = element_text(family = font_family,face=font_face,size = size_axis_title,colour = "black"),
            axis.title.y = element_text(family = font_family,face=font_face,size=size_axis_title,colour="black"),
            legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
            legend.title=element_text(size=size_title_text,
        family = font_family,face = font_face,colour = "black"),
            legend.key = element_blank(),
            legend.text = element_text(size=size_legend_text,
                                       family = font_family,face = font_face,colour = "black"),
            legend.position ="right")
    p <- p + scale_color_manual(values = mix.colors)

    #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
    return(p)
  }

  }
}
