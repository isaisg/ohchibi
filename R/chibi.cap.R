#' Plots results from oh.cap
#'
#' Uses the structure return from oh.cap to automatically plot a scatterplot colored by variables from the Map structured used to compute oh.cap
#' @keywords cap
#' @export
#' @examples
#' chibi.cap()


chibi.cap<-function(list_ohpco=NULL,col_val=NULL,shape_val=NULL,comp_a=NULL,comp_b=NULL,mypch=21,
        size=25,alpha=0.7,stroke=1.5,col_shape_background="white",alpha_shape_background=0,
        ratio_size_shape_background=1.3,y_vjust=0.5,x_hjust=0.5,size_axis_text=20,size_axis_title=30,
        size_legend_text=20,legend_proportion_size=2,size_lines_panel = 0.3,size_panel_border = 1,font_family = "AvantGarde"){
  percvar<-list_ohpco$variance_explained_axis
  Map_cap<-list_ohpco$Map_cap
  vartot<-list_ohpco$total_var
  pval<-list_ohpco$pval_model
    if(is.null(shape_val)){
    p <- ggplot(data = Map_cap,aes_string(x = comp_a,y = comp_b))+
      geom_point(size = size,alpha=alpha,pch=mypch,colour="#414141",stroke = stroke,aes_string(fill = col_val))+
      geom_point(size = size/ratio_size_shape_background,colour = col_shape_background,alpha=alpha_shape_background)+ 
      xlab(label = paste(comp_a,"(",percvar[which(names(percvar)==comp_a)],"%)",sep="")) + 
      ylab(label = paste(comp_b,"(",percvar[which(names(percvar)==comp_b)],"%)",sep="")) +
      guides(fill = guide_legend(override.aes = list(size=12))) +
      theme(axis.line = element_blank(),
            panel.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(colour =   "#D9D9D9",size = size_lines_panel),
            panel.grid.minor = element_line(colour = "#D9D9D9", size = size_lines_panel),
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
            legend.position ="right") 
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
              legend.position ="right") 
      #p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
      return(p)
    }
}

