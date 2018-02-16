#' Creates a boxplot ussing ggplot2 using variables in a metadata (Map) structure.
#'
#' The boxplot can represent up to three variables from the Map object
#' @keywords boxplot
#' @export
#' @examples
#' chibi.boxplot()


chibi.boxplot<-function(Map=Map,x_val=NULL,y_val=NULL,col_val=NULL,shape_val=NULL,style="mixed",mpalette=NULL,color_boxplot="#414141",median_color="red",mypch_point=21,size_point=3,alpha_point=0.5,stroke_point=0.5,size_boxplot=0.5,size_median=2){
  if(is.null(mpalette)){
    mpalette <- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666")
  }
  if(style == "full"){
    if(is.null(col_val)){
      stop("ERROR: Need to provide a col_val when using the full style",call.=TRUE)
    }else{
      if(is.null(mpalette)){
        mpalette <- mpalette[1:length(levels(Map[,which(colnames(Map)==col_val)]))]
      }
      p <- ggplot(Map, aes_string(x = x_val, y = y_val,fill = col_val)) + 
        geom_boxplot(color=color_boxplot, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +  
        theme(axis.line = element_blank(),
              panel.background = element_rect(fill = 'white'),
              panel.grid.major.x =  element_blank(),
              panel.grid.major.y =element_line(colour = "#D9D9D9",size=0.3),
              panel.grid.minor.y = element_line(colour = "#D9D9D9",size=0.3),
              panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
              axis.ticks = element_line(colour = "black",size = 2.5),
              axis.text.x = element_text(family = "AvantGarde",face = "plain",size =20,colour="#414141",angle = 0),
              axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
              axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
              axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
              legend.background = element_blank(),legend.key.size = unit(2,"line"),
              legend.title=element_blank(),legend.key = element_blank(), 
              legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
              legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
              strip.background = element_rect(fill = "#D9D9D9",color = "#414141")) 
      p <- p + scale_fill_manual(values = mpalette)
      if(is.null(shape_val)){
        dat <- ggplot_build(p)$data[[1]]
        p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                          y=middle, yend=middle), colour=median_color, size=size_median,inherit.aes = F)
       p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
        jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
       myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
       x_interval <- 1:myrep
       limit_x <- length(x_interval)-1
       x_interval <- x_interval[1:limit_x]+0.5
       p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)

      }else{
        dat <- ggplot_build(p)$data[[1]]
        p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                            y=middle, yend=middle), colour=median_color, size=size_median,inherit.aes = F)
        p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                          size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,shape=shape_val),inherit.aes = F)
        p <- p + scale_color_manual(values = mpalette)
        myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
        x_interval <- 1:myrep
        limit_x <- length(x_interval)-1
        x_interval <- x_interval[1:limit_x]+0.5
        p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)

      }
    }
  }else if(style == "open"){
      if(is.null(col_val)){
        p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
          geom_boxplot(color=color_boxplot,outlier.colour = NA,position = position_dodge(width = 0.9), size=size_boxplot) +  
          theme(axis.line = element_blank(),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major.x =  element_blank(),
                panel.grid.major.y =element_line(colour = "#D9D9D9",size=0.3),
                panel.grid.minor.y = element_line(colour = "#D9D9D9",size=0.3),
                panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
                axis.ticks = element_line(colour = "black",size = 2.5),
                axis.text.x = element_text(family = "AvantGarde",face = "plain",size =20,colour="#414141",angle = 0),
                axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
                axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
                axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
                legend.background = element_blank(),legend.key.size = unit(2,"line"),
                legend.title=element_blank(),legend.key = element_blank(), 
                legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
                legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
                strip.background = element_rect(fill = "#D9D9D9",color = "#414141")) 
        if(is.null(shape_val)){
          dat <- ggplot_build(p)$data[[1]]
          p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                         y=middle, yend=middle), colour=median_color, size=size_median)
          p  <- p + geom_jitter(position = position_jitter(0.2),
              size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)
          myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
          x_interval <- 1:myrep
          limit_x <- length(x_interval)-1
          x_interval <- x_interval[1:limit_x]+0.5
          p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
   
        }else{
          dat <- ggplot_build(p)$data[[1]]
          p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                               y=middle, yend=middle), colour=median_color, size=size_median)
          p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                            size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,shape=shape_val),color="#414141",inherit.aes = F)
          myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
          x_interval <- 1:myrep
          limit_x <- length(x_interval)-1
          x_interval <- x_interval[1:limit_x]+0.5
          p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)

        }
        
      }else{
        if(is.null(mpalette)){
          mpalette <- mpalette[1:length(levels(Map[,which(colnames(Map)==col_val)]))]
        }
        p <- ggplot(Map, aes_string(x = x_val, y = y_val, col = col_val,  fill = col_val)) + 
          geom_boxplot(fill = NA, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +  
          theme(axis.line = element_blank(),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major.x =  element_blank(),
                panel.grid.major.y =element_line(colour = "#D9D9D9",size=0.3),
                panel.grid.minor.y = element_line(colour = "#D9D9D9",size=0.3),
                panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
                axis.ticks = element_line(colour = "black",size = 2.5),
                axis.text.x = element_text(family = "AvantGarde",face = "plain",size =20,colour="#414141",angle = 0),
                axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
                axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
                axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
                legend.background = element_blank(),legend.key.size = unit(2,"line"),
                legend.title=element_blank(),legend.key = element_blank(), 
                legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
                legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
                strip.background = element_rect(fill = "#D9D9D9",color = "#414141"))  
        if(is.null(shape_val)){
          dat <- ggplot_build(p)$data[[1]]
          p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                              y=middle, yend=middle), colour=median_color, size=size_median,inherit.aes = F)
          p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
            jitter.width = 0.1), size = size_point, shape = mypch_point, col = "#414141",stroke=stroke_point,alpha=alpha_point)
          p <- p + scale_fill_manual(values = mpalette) + scale_color_manual(values = mpalette)
          myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
          x_interval <- 1:myrep
          limit_x <- length(x_interval)-1
          x_interval <- x_interval[1:limit_x]+0.5
          p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)

        }else{
          dat <- ggplot_build(p)$data[[1]]
          p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                              y=middle, yend=middle), colour=median_color, size=size_median,inherit.aes = F)
          p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                            size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,shape=shape_val),inherit.aes = F)
          p <- p + scale_fill_manual(values = mpalette) + scale_color_manual(values = mpalette)
          

        }
      }
  }else if (style == "mix"){
    if(is.null(mpalette)){
        mpalette <- mpalette[1:length(levels(Map[,which(colnames(Map)==col_val)]))]
    }
    if(is.null(shape_val)){
        myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
        p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
          geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
          scale_color_manual(values = rep("#414141",myrep)) + 
          geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                     size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
          scale_fill_manual(values = mpalette)+
          theme(axis.line = element_blank(),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major.x =  element_blank(),
                panel.grid.major.y =element_line(colour = "#D9D9D9",size=0.3),
                panel.grid.minor.y = element_line(colour = "#D9D9D9",size=0.3),
                panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
                axis.ticks = element_line(colour = "black",size = 2.5),
                axis.text.x = element_text(family = "AvantGarde",face = "plain",size =20,colour="#414141",angle = 0),
                axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
                axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
                axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
                legend.background = element_blank(),legend.key.size = unit(2,"line"),
                legend.title=element_blank(),legend.key = element_blank(), 
                legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
                legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
                strip.background = element_rect(fill = "#D9D9D9",color = "#414141")) 
        
        dat <- ggplot_build(p)$data[[1]]
        map_melted <- melt(table(Map[,which(colnames(Map)==x_val)],Map[,which(colnames(Map)==col_val)]))
        colnames(map_melted)[1:2] <- c(x_val,col_val)
        map_melted <- map_melted[which(map_melted$value!=0),]
        map_melted <- map_melted[order(map_melted[,1]),]
        refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
        mcolors <- NULL
        for(level in levels(map_melted[,1])){
          sub_temp <- map_melted[which(map_melted[,1]==level),]
          mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
        }
        dat$colour <- mcolors
        p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                            y=middle, yend=middle),colour=dat$colour,size=size_median)
        
    
        x_interval <- 1:myrep
        limit_x <- length(x_interval)-1
        x_interval <- x_interval[1:limit_x]+0.5
        
        p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
      }else{
        stop("ERROR: Shape_val is not implemented in the mix style. Use the open style to better plot shape_val",call.=TRUE)
        
      }
      
  }else{
    stop("ERROR: Unrecognized style, try mix (default), full or open.",call.=TRUE)
    
    }

  return(p)
}

