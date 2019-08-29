#' Plots results from oh.pco
#'
#' Uses the variables in the metadata (Map) to color or shape the ggplot based graph.
#' @keywords pco
#' @export
#' @examples
#' chibi.pco()


chibi.pco<-function(list_ohpco=NULL,col_val=NULL,shape_val=NULL,comp_a="PCo1",comp_b="PCo2",mypch=21,
                    size=25,alpha=0.7,stroke=1.5,col_shape_background="white",alpha_shape_background=0,
                    lines_zero = FALSE,size_axis_line=0.5,type_axis_line = "longdash",
                    ratio_size_shape_background=1.3,y_vjust=0.5,x_hjust=0.5,size_axis_text=20,size_axis_title=30,
                    size_legend_text=20,size_title_text = 30,legend_proportion_size=2,
		size_lines_panel = 0,size_panel_border = 1,font_family = "Arial"){
  mix.colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928")
  Map_pco<-list_ohpco$Map_pco
  myvar=list_ohpco$variance_explained
  if(lines_zero == FALSE) {
    if(is.null(shape_val)){
      p <- ggplot(data = Map_pco,aes_string(x = comp_a,y = comp_b))+
        geom_point(size = size,alpha=alpha,pch=mypch,colour="black",stroke = stroke,aes_string(fill = col_val))+
        geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+
        scale_fill_manual(values = mix.colors)+
        xlab(label = paste(comp_a,"(",myvar[which(names(myvar)==comp_a)],"%)",sep="")) +
        ylab(label = paste(comp_b,"(",myvar[which(names(myvar)==comp_b)],"%)",sep="")) +
        guides(fill = guide_legend(override.aes = list(size=12))) +
        theme(axis.line = element_blank(),
              panel.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
              panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
              panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
              axis.ticks = element_line(colour = "black",size = 2.5),
              axis.text.x = element_text(family = font_family,face = "bold",size =size_axis_text,colour="black",hjust = x_hjust),
              axis.text.y = element_text(family = font_family,face="bold",size=size_axis_text,colour="black",vjust = y_vjust),
              axis.title.x = element_text(family = font_family,face="bold",size = size_axis_title,colour = "black"),
              axis.title.y = element_text(family = font_family,face="bold",size=size_axis_title,colour="black"),
              legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
              legend.title=element_text(size=size_title_text,
            family = font_family,face = "bold",colour = "black"),            
	    #legend.title = element_blank(),
	  legend.key = element_blank(),
              legend.text = element_text(size=size_legend_text,
                                         family = font_family,face = "bold",colour = "black"),
              legend.position ="right")
      #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
      return(p)
    }else{
      p <- ggplot(data = Map_pco,aes_string(x = comp_a,y = comp_b,shape=shape_val))+
        geom_point(size = size,alpha=alpha,aes_string(colour = col_val))+
        geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+
        xlab(label = paste(comp_a,"(",myvar[which(names(myvar)==comp_a)],"%)",sep="")) +
        ylab(label = paste(comp_b,"(",myvar[which(names(myvar)==comp_b)],"%)",sep="")) +
        guides(fill = guide_legend(override.aes = list(size=12))) +
        theme(axis.line = element_blank(),
              panel.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
              panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
              panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
              axis.ticks = element_line(colour = "black",size = 2.5),
              axis.text.x = element_text(family = font_family,face = "bold",size =size_axis_text,colour="black",hjust = x_hjust),
              axis.text.y = element_text(family = font_family,face="bold",size=size_axis_text,colour="black",vjust = y_vjust),
              axis.title.x = element_text(family = font_family,face="bold",size = size_axis_title,colour = "black"),
              axis.title.y = element_text(family = font_family,face="bold",size=size_axis_title,colour="black"),
              legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
              legend.title=element_text(size=size_title_text,family = font_family,face = "bold",colour = "black"),            
	   #legend.title = element_blank(),
	legend.key = element_blank(),
              legend.text = element_text(size=size_legend_text,
                                         family = font_family,face = "bold",colour = "black"),
              legend.position ="right")
      #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
      return(p)
    }
  }else{
  if(is.null(shape_val)){
    p <- ggplot(data = Map_pco,aes_string(x = comp_a,y = comp_b))+
    geom_vline(xintercept = 0,size = size_axis_line,color = "#D9D9D9",linetype = type_axis_line) +
    geom_hline(yintercept = 0,size = size_axis_line,color = "#D9D9D9",linetype = type_axis_line) +
      geom_point(size = size,alpha=alpha,pch=mypch,colour="black",stroke = stroke,aes_string(fill = col_val))+
      geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+
      scale_fill_manual(values = mix.colors)+
      xlab(label = paste(comp_a,"(",myvar[which(names(myvar)==comp_a)],"%)",sep="")) +
      ylab(label = paste(comp_b,"(",myvar[which(names(myvar)==comp_b)],"%)",sep="")) +
      guides(fill = guide_legend(override.aes = list(size=12))) +
      theme(axis.line = element_blank(),
            panel.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
            panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
            panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
            axis.ticks = element_line(colour = "black",size = 2.5),
            axis.text.x = element_text(family = font_family,face = "bold",size =size_axis_text,colour="black",hjust = x_hjust),
            axis.text.y = element_text(family = font_family,face="bold",size=size_axis_text,colour="black",vjust = y_vjust),
            axis.title.x = element_text(family = font_family,face="bold",size = size_axis_title,colour = "black"),
            axis.title.y = element_text(family = font_family,face="bold",size=size_axis_title,colour="black"),
            legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
            legend.title=element_text(size=size_title_text,family = font_family,face = "bold",colour = "black"),
	    #legend.title = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size=size_legend_text,
                                       family = font_family,face = "bold",colour = "black"),
            legend.position ="right")
    #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
    return(p)
  }else{
    p <- ggplot(data = Map_pco,aes_string(x = comp_a,y = comp_b,shape=shape_val))+
    geom_vline(xintercept = 0,size = size_axis_line,color = "#D9D9D9",linetype = type_axis_line) +
    geom_hline(yintercept = 0,size = size_axis_line,color = "#D9D9D9",linetype = type_axis_line) +
      geom_point(size = size,alpha=alpha,aes_string(colour = col_val))+
      geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+
      xlab(label = paste(comp_a,"(",myvar[which(names(myvar)==comp_a)],"%)",sep="")) +
      ylab(label = paste(comp_b,"(",myvar[which(names(myvar)==comp_b)],"%)",sep="")) +
      guides(fill = guide_legend(override.aes = list(size=12))) +
      theme(axis.line = element_blank(),
            panel.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
            panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
            panel.border = element_rect(fill=NA,color =  "black",size = size_panel_border),
            axis.ticks = element_line(colour = "black",size = 2.5),
            axis.text.x = element_text(family = font_family,face = "bold",size =size_axis_text,colour="black",hjust = x_hjust),
            axis.text.y = element_text(family = font_family,face="bold",size=size_axis_text,colour="black",vjust = y_vjust),
            axis.title.x = element_text(family = font_family,face="bold",size = size_axis_title,colour = "black"),
            axis.title.y = element_text(family = font_family,face="bold",size=size_axis_title,colour="black"),
            legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
            legend.title=element_text(size=size_title_text,family = font_family,face = "bold",colour = "black"),
	    #legend.title = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size=size_legend_text,
                                       family = font_family,face = "bold",colour = "black"),
            legend.position ="right")
    #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
    return(p)
  }
  }
}
