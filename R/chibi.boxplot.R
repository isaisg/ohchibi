#' Creates a boxplot ussing ggplot2 using variables in a metadata (Map) structure.
#'
#' The boxplot can represent up to three variables from the Map object
#' @keywords boxplot
#' @export
#' @examples
#' chibi.boxplot()


chibi.boxplot<-function(Map=Map,x_val=NULL,y_val=NULL,col_val=NULL,shape_val=NULL,style="full",color_boxplot="#414141",mypch_point=21,size_point=8,alpha_point=1,stroke_point=0.5,size_boxplot=2){
  #Check the overall style
  if(style == "full"){
    if(col_val = NULL){
      #Return error
    }else{
      p <- ggplot(Map, aes_string(x = x_val, y = y_val,fill = col_val)) + 
        geom_boxplot(color=color_boxplot, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +  
        theme(axis.line = element_blank(),
              panel.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(colour =   "#D9D9D9"),
              panel.grid.minor = element_line(colour = "#D9D9D9"),
              panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
              axis.ticks = element_line(colour = "black",size = 2.5),
              axis.text.x = element_text(family = "AvantGarde",face = "plain",size =20,colour="#414141",angle = 90),
              axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
              axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
              axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
              legend.background = element_blank(),legend.key.size = unit(2,"line"),
              legend.title=element_blank(),legend.key = element_blank(), 
              legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
              legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
              strip.background = element_rect(fill = "#D9D9D9",color = "#414141")) 
      #Evaluate if shaped need to be added
      if(is.null(shape_val)){
        p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
        jitter.width = 0.1), size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)
        #Define the guides. They gotta be constant
        p <- p +guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",
                                         override.aes = list(size=3,stroke=stroke_point,shape=mypch_point,alpha=alpha_point)))
        
      }else{
        p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                          size = size_point,col = color_boxplot,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,fill=col_val,shape=shape_val),inherit.aes = F)
        p <- p +guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=3,stroke=stroke_point,alpha=alpha_point)))
        
      }
    }
  }else{
    if(style == "open"){
      if(col_val == NULL){
        p <- ggplot(Dat_rar$Map, aes_string(x = "Fraction", y = "Shannon")) + 
          geom_boxplot(color=color_boxplot,outlier.colour = NA,position = position_dodge(width = 0.9), size=size_boxplot) +  
          theme(axis.line = element_blank(),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major = element_line(colour =   "#D9D9D9"),
                panel.grid.minor = element_line(colour = "#D9D9D9"),
                panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
                axis.ticks = element_line(colour = "black",size = 2.5),
                axis.text.x = element_text(family = "AvantGarde",face = "plain",size =20,colour="#414141",angle = 90),
                axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
                axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
                axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
                legend.background = element_blank(),legend.key.size = unit(2,"line"),
                legend.title=element_blank(),legend.key = element_blank(), 
                legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
                legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
                strip.background = element_rect(fill = "#D9D9D9",color = "#414141")) 
        #Evaluate if shaped need to be added
        if(is.null(shape_val)){
          p  <- p + geom_jitter(position = position_jitter(0.2),
              size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)
          #Define the guides. They gotta be constant
          p <- p +guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",
            override.aes = list(size=3,stroke=stroke_point,shape=mypch_point,alpha=alpha_point)))
          
        }else{
          p<- p + geom_jitter(position = position_jitter(0.2), 
                size = size_point,col=color_boxplot,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,shape=shape_val),inherit.aes = F)
          p <- p +guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=3,stroke=stroke_point,alpha=alpha_point)))
          
        }
        
      }else{
        p <- ggplot(Map, aes_string(x = x_val, y = y_val, col = col_val,  fill = col_val)) + 
          geom_boxplot(fill = NA, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +  
          theme(axis.line = element_blank(),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major = element_line(colour =   "#D9D9D9"),
                panel.grid.minor = element_line(colour = "#D9D9D9"),
                panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
                axis.ticks = element_line(colour = "black",size = 2.5),
                axis.text.x = element_text(family = "AvantGarde",face = "plain",size =20,colour="#414141",angle = 90),
                axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
                axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
                axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
                legend.background = element_blank(),legend.key.size = unit(2,"line"),
                legend.title=element_blank(),legend.key = element_blank(), 
                legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
                legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
                strip.background = element_rect(fill = "#D9D9D9",color = "#414141")) 
        #Evaluate if shaped need to be added
        if(is.null(shape_val)){
          p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
            jitter.width = 0.1), size = size_point, shape = mypch_point, col = "#414141",stroke=stroke_point,alpha=alpha_point)
          #Define the guides. They gotta be constant
          p <- p +guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",
                override.aes = list(size=3,stroke=stroke_point,shape=mypch_point,alpha=alpha_point)))
          
        }else{
          p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                            size = size_point,col = "#414141",stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,fill=col_val,shape=shape_val),inherit.aes = F)
          p <- p +guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=3,stroke=stroke_point,alpha=alpha_point)))
          
        }
      }
    }else{
      #Print error does not understand that style
      
    }
  }

  return(p)
}

