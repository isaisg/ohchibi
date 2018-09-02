#' Creates a boxplot ussing ggplot2 using variables in a metadata (Map) structure.
#'
#' The boxplot can represent up to three variables from the Map object
#' @keywords boxplot
#' @export
#' @examples
#' chibi.boxplot()


chibi.boxplot<-function(Map=Map,x_val=NULL,y_val=NULL,col_val=NULL,shape_val=NULL,style="mix",
                        median_colored_as_points = FALSE,
                        facet_formula=NULL,mpalette=NULL,color_boxplot="#414141",median_color="red",
                        mypch_point=21,size_point=4,
                        alpha_point=0.5,stroke_point=0.5,size_boxplot=0.5,size_median=2,
                        size_axis_text.x=20,size_axis_text.y=20,size_axis_title.x=30,size_axis_title.y=30,
                        size_legend_text=20,size_title_text = 30,strip_text_size=20,legend_proportion_size=2,
			size_lines_panel = 0.3,size_panel_border = 1,font_family = "Arial"){
  if(is.null(mpalette)){
    mpalette <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
      "#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928")
   # mpalette <- mpalette[1:length(levels(Map[,which(colnames(Map)==col_val)]))]
    
  }
  #Shape value should only make sense in the open style
  if(! is.null(shape_val) & style!="open"){
    stop("ERROR: the shape_val plotting is implement only in the open style. please change style=open",call.=TRUE)
    
  }
  #Check if facet is requested
  if(! is.null(facet_formula)){
    facet_formula <- gsub(pattern = " ",replacement = "",x = facet_formula)
    lfacet <- length(unlist(strsplit(x = facet_formula,split = "\\+")))
    facet_vals <- unlist(strsplit(x = facet_formula,split = "\\+"))
    facet_formula <- as.formula(paste("~", facet_formula))
  }

  if(style == "full"){
    if(is.null(col_val)){
      stop("ERROR: Need to provide a col_val when using the full style",call.=TRUE)
    }else{
      if(is.null(facet_formula)){
        ########################
        ########################
        ##Full Style no facet###
        ########################
        ########################
        p <- ggplot(Map, aes_string(x = x_val, y = y_val,fill = col_val)) + 
          geom_boxplot(color=color_boxplot, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) 
        p <- p + scale_fill_manual(values = mpalette)
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
        if(median_colored_as_points == FALSE){
          dat$colour <- rep(median_color,nrow(dat))
          
        }else{
          dat$colour <- mcolors
          
        }
        p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                            y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
        p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                          jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
        

      }else{
        ########################
        ########################
        ##Full Style facet #####
        ##### col_val!=x_val####
        ########################
        ########################
        if(col_val != x_val){
          if(lfacet == 1){
            p <- ggplot(Map, aes_string(x = x_val, y = y_val,fill = col_val)) + 
              geom_boxplot(color=color_boxplot, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) 
            p <- p + scale_fill_manual(values = mpalette)
            p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
            red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
            red_formula <- as.formula(paste(red_formula,"+",as.character(col_val),"~",as.character(x_val),sep = " "))
            dat <- ggplot_build(p)$data[[1]]
            map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                               id.vars = c(1,2),,variable.name = x_val)
            map_melted <- map_melted[which(map_melted$value!=0),]
            refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
            int_cols <- which(colnames(map_melted)!=col_val)
            map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]]),]
            combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],sep = "_")
            map_melted$Combined <- factor(combined,levels = unique(combined))
            dat <- cbind(dat,map_melted)
            mcolors <- NULL
            for(level in levels(map_melted$Combined)){
              sub_temp <- map_melted[which(map_melted$Combined==level),]
              mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
            }
            if(median_colored_as_points == FALSE){
              dat$colour <- rep(median_color,nrow(dat))
            }else{
              dat$colour <- mcolors
              
            }
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                              jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
            
            
          }else if (lfacet ==2){
            p <- ggplot(Map, aes_string(x = x_val, y = y_val,fill = col_val)) + 
              geom_boxplot(color=color_boxplot, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot)
            p <- p + scale_fill_manual(values = mpalette)
            p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
            red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
            red_formula <- as.formula(paste(red_formula,"+",as.character(col_val),"~",as.character(x_val),sep = " "))
            dat <- ggplot_build(p)$data[[1]]
            map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                               id.vars = c(1,2,3),,variable.name = x_val)
            map_melted <- map_melted[which(map_melted$value!=0),]
            refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
            int_cols <- which(colnames(map_melted)!=col_val)
            map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]],map_melted[,int_cols[3]]),]
            combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],map_melted[,int_cols[3]],sep = "_")
            map_melted$Combined <- factor(combined,levels = unique(combined))
            dat <- cbind(dat,map_melted)
            mcolors <- NULL
            for(level in levels(map_melted$Combined)){
              sub_temp <- map_melted[which(map_melted$Combined==level),]
              mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,3],refdf[,1]),2])))
            }
            if(median_colored_as_points == FALSE){
              dat$colour <- rep(median_color,nrow(dat))
            }else{
              dat$colour <- mcolors
              
            }
            
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                              jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
            
            
          }else{
            stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment constructing the figure manually",call.=TRUE)
            
          }
        }else{
          ########################
          ########################
          ##Full Style facet #####
          ##### col_val==x_val####
          ########################
          ########################
          if(lfacet == 1){
            p <- ggplot(Map, aes_string(x = x_val, y = y_val,fill = col_val)) + 
              geom_boxplot(color=color_boxplot, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) 
            p <- p + scale_fill_manual(values = mpalette)
            p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
            #Change this part when facette is equal two
            red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
            red_formula <- as.formula(paste(red_formula,"~",as.character(col_val),sep = " "))
            dat <- ggplot_build(p)$data[[1]]
            map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                               id.vars = c(1),variable.name = x_val)
            map_melted <- map_melted[which(map_melted$value!=0),]
            refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
            #change the next three lines when facette is equal to two 
            int_cols <- 1
            map_melted <- map_melted[order(map_melted[,int_cols[1]]),]
            combined <- paste(map_melted[,int_cols[1]],sep = "_")
            map_melted$Combined <- factor(combined,levels = unique(combined))
            dat <- cbind(dat,map_melted)
            mcolors <- NULL
            for(level in levels(map_melted$Combined)){
              sub_temp <- map_melted[which(map_melted$Combined==level),]
              mcolors <- c(mcolors,(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
            }
            if(median_colored_as_points == FALSE){
              dat$colour <- rep(median_color,nrow(dat))
            }else{
              dat$colour <- mcolors
            }
            
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                              jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
            
            
            
          }else if (lfacet ==2){
            p <- ggplot(Map, aes_string(x = x_val, y = y_val,fill = col_val)) + 
              geom_boxplot(color=color_boxplot, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) 
            p <- p + scale_fill_manual(values = mpalette)
            p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
            #Change this part when facette is equal two
            red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
            red_formula <- as.formula(paste(red_formula,"~",as.character(col_val),sep = " "))
            dat <- ggplot_build(p)$data[[1]]
            map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                               id.vars = c(1,2),variable.name = x_val)
            map_melted <- map_melted[which(map_melted$value!=0),]
            refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
            #change the next three lines when facette is equal to two 
            int_cols <- c(1,2)
            map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]]),]
            combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],sep = "_")
            map_melted$Combined <- factor(combined,levels = unique(combined))
            dat <- cbind(dat,map_melted)
            mcolors <- NULL
            for(level in levels(map_melted$Combined)){
              sub_temp <- map_melted[which(map_melted$Combined==level),]
              mcolors <- c(mcolors,(as.character(refdf[match(sub_temp[,3],refdf[,1]),2])))
            }
            if(median_colored_as_points == FALSE){
              dat$colour <- rep(median_color,nrow(dat))
            }else{
              dat$colour <- mcolors
            }
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                              jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
            

            
          }else{
            stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment constructing the figure manually",call.=TRUE)
            
          }
        }
 
      }
   
 
    }
  }else if(style == "open"){
      if(is.null(col_val)){
        if(is.null(facet_formula)){
          ########################
          ########################
          ##Open Style no facet###
          ###### No color ########
          ########################
          ########################
          #This is the black style with red medians. Great when there are a lot of points such as RNA-Seq
          p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
            geom_boxplot(color=color_boxplot,outlier.colour = NA,position = position_dodge(width = 0.9), size=size_boxplot)
          dat <- ggplot_build(p)$data[[1]]
          p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                              y=middle, yend=middle), colour=median_color, size=size_median)
          if(is.null(shape_val)){
            p  <- p + geom_jitter(position = position_jitter(0.2),
                                  size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)
            
            
          }else{
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                              size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,shape=shape_val),color="#414141",inherit.aes = F)
            
          }
          
        }else{
          ########################
          ########################
          ##Open Style with facet#
          ###### No color ########
          ########################
          ########################
          if(lfacet == 1){
            p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
              geom_boxplot(color=color_boxplot,outlier.colour = NA,position = position_dodge(width = 0.9), size=size_boxplot) 
            p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
            red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
            red_formula <- as.formula(paste(red_formula,"~",as.character(x_val),sep = " "))
            dat <- ggplot_build(p)$data[[1]]
            map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                               id.vars = c(1),variable.name = x_val)
            map_melted <- map_melted[which(map_melted$value!=0),]
            refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==x_val)]),colors=mpalette)
            int_cols <- c(1,2)
            map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]]),]
            combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],sep = "_")
            map_melted$Combined <- factor(combined,levels = unique(combined))
            dat <- cbind(dat,map_melted)
            mcolors <- NULL
            for(level in levels(map_melted$Combined)){
              sub_temp <- map_melted[which(map_melted$Combined==level),]
              mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
            }
            dat$colour <- rep(median_color,nrow(dat))
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            
            if(is.null(shape_val)){
              p  <- p + geom_jitter(position = position_jitter(0.2),
                                    size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)
              
            }else{
             
              p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                                size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,shape=shape_val),color="#414141",inherit.aes = F)
              
            }
          }else if (lfacet ==2){
            #Open style no color when facetting equal to 2
            p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
              geom_boxplot(color=color_boxplot,outlier.colour = NA,position = position_dodge(width = 0.9), size=size_boxplot) 
            p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
            red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
            red_formula <- as.formula(paste(red_formula,"~",as.character(x_val),sep = " "))
            dat <- ggplot_build(p)$data[[1]]
            map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                               id.vars = c(1,2),variable.name = x_val)
            map_melted <- map_melted[which(map_melted$value!=0),]
            refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==x_val)]),colors=mpalette)
            int_cols <- c(1,2,3)
            map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]],map_melted[,int_cols[3]]),]
            combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],map_melted[,int_cols[3]],sep = "_")
            map_melted$Combined <- factor(combined,levels = unique(combined))
            dat <- cbind(dat,map_melted)
            mcolors <- NULL
            for(level in levels(map_melted$Combined)){
              sub_temp <- map_melted[which(map_melted$Combined==level),]
              mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
            }
            dat$colour <- rep(median_color,nrow(dat))
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            if(is.null(shape_val)){

              p  <- p + geom_jitter(position = position_jitter(0.2),
                                    size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)
              
            }else{
            
              p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                                size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,shape=shape_val),color="#414141",inherit.aes = F)

            }
            
          }else{
            stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment constructing the figure manually",call.=TRUE)
            
          }
        }
       
      }else{
        ########################
        ########################
        ##Open Style no facet###
        ###### With color ######
        ####col_val!=x_val######
        ########################
        ########################
        if(col_val != x_val){
          if(is.null(facet_formula)){
            p <- ggplot(Map, aes_string(x = x_val, y = y_val, col = col_val,  fill = col_val)) + 
              geom_boxplot(fill = NA, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
              geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                         size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)+
              scale_color_manual(values = mpalette)+ scale_fill_manual(values = mpalette)
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
            if(median_colored_as_points == FALSE){
              dat$colour <- rep(median_color,nrow(dat))
            }else{
              dat$colour <- mcolors
            }
            if(is.null(shape_val)){
             
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
              
              
              
            }else{
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                  y=middle, yend=middle), colour=dat$colour, size=size_median,inherit.aes = F)
              p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                                size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,shape=shape_val),inherit.aes = F)
            }
            
          }else{
            ########################
            ########################
            ##Open Style with facet#
            ###### With color ######
            ####col_val!=x_val######
            ########################
            ########################
            if(lfacet == 1){
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = mpalette)+
                geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                           size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
                scale_fill_manual(values = mpalette)
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"+",as.character(col_val),"~",as.character(x_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1,2),,variable.name = x_val)
              map_melted <- map_melted[which(map_melted$value!=0),]
              refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
              int_cols <- which(colnames(map_melted)!=col_val)
              map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]]),]
              combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],sep = "_")
              map_melted$Combined <- factor(combined,levels = unique(combined))
              dat <- cbind(dat,map_melted)
              mcolors <- NULL
              for(level in levels(map_melted$Combined)){
                sub_temp <- map_melted[which(map_melted$Combined==level),]
                mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
              }
              if(median_colored_as_points == FALSE){
                dat$colour <- rep(median_color,nrow(dat))
              }else{
                dat$colour <- mcolors
              }
              
              if(is.null(shape_val)){
                
                p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                    y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
                
                
                
              }else{
                p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                    y=middle, yend=middle), colour=dat$colour, size=size_median,inherit.aes = F)
                p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                                  size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,shape=shape_val),inherit.aes = F)
              }
              
            }else if (lfacet ==2){
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = mpalette)+
                geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                           size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
                scale_fill_manual(values = mpalette)
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"+",as.character(col_val),"~",as.character(x_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1,2,3),,variable.name = x_val)
              map_melted <- map_melted[which(map_melted$value!=0),]
              refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
              int_cols <- which(colnames(map_melted)!=col_val)
              map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]],map_melted[,int_cols[3]]),]
              combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],map_melted[,int_cols[3]],sep = "_")
              map_melted$Combined <- factor(combined,levels = unique(combined))
              dat <- cbind(dat,map_melted)
              mcolors <- NULL
              for(level in levels(map_melted$Combined)){
                sub_temp <- map_melted[which(map_melted$Combined==level),]
                mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,3],refdf[,1]),2])))
              }
              if(median_colored_as_points == FALSE){
                dat$colour <- rep(median_color,nrow(dat))
              }else{
                dat$colour <- mcolors
              }
              if(is.null(shape_val)){
                
                p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                    y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
                
                
                
              }else{
                p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                    y=middle, yend=middle), colour=dat$colour, size=size_median,inherit.aes = F)
                p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                                  size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,shape=shape_val),inherit.aes = F)
              }
            }else{
              stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment constructing the figure manually",call.=TRUE)
              
            }
          }
        }else{
          ########################
          ########################
          ##Open Style no facet###
          ###### With color ######
          ####col_val==x_val######
          ########################
          ########################
          if(is.null(facet_formula)){
            p <- ggplot(Map, aes_string(x = x_val, y = y_val, col = col_val,  fill = col_val)) + 
              geom_boxplot(fill = NA, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
              geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                         size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)+
              scale_color_manual(values = mpalette)+ scale_fill_manual(values = mpalette)
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
            if(median_colored_as_points == FALSE){
              dat$colour <- rep(median_color,nrow(dat))
            }else{
              dat$colour <- mcolors
            }
            if(is.null(shape_val)){
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
              
              
              
            }else{
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                  y=middle, yend=middle), colour=dat$colour, size=size_median,inherit.aes = F)
              p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                                size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,shape=shape_val),inherit.aes = F)
            }

          }else{
            ########################
            ########################
            ##Open Style with facet#
            ###### With color ######
            ####col_val==x_val######
            ########################
            ########################
            if(lfacet == 1){
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = mpalette)+
                geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                           size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
                scale_fill_manual(values = mpalette)
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"~",as.character(col_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1),,variable.name = x_val)
              map_melted <- map_melted[which(map_melted$value!=0),]
              refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
              int_cols <- 1
              map_melted <- map_melted[order(map_melted[,int_cols[1]]),]
              combined <- paste(map_melted[,int_cols[1]],sep = "_")
              map_melted$Combined <- factor(combined,levels = unique(combined))
              dat <- cbind(dat,map_melted)
              mcolors <- NULL
              for(level in levels(map_melted$Combined)){
                sub_temp <- map_melted[which(map_melted$Combined==level),]
                mcolors <- c(mcolors,(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
              }
              if(median_colored_as_points == FALSE){
                dat$colour <- rep(median_color,nrow(dat))
              }else{
                dat$colour <- mcolors
              }
              
              if(is.null(shape_val)){
                
                p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                    y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
                
                
                
              }else{
                p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                    y=middle, yend=middle), colour=dat$colour, size=size_median,inherit.aes = F)
                p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                                  size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,shape=shape_val),inherit.aes = F)
              }
              
            }else if (lfacet ==2){
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = mpalette)+
                geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                           size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
                scale_fill_manual(values = mpalette)
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"~",as.character(col_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1,2),,variable.name = x_val)
              map_melted <- map_melted[which(map_melted$value!=0),]
              refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
              int_cols <- c(1,2)
              map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]]),]
              combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],sep = "_")
              map_melted$Combined <- factor(combined,levels = unique(combined))
              dat <- cbind(dat,map_melted)
              mcolors <- NULL
              for(level in levels(map_melted$Combined)){
                sub_temp <- map_melted[which(map_melted$Combined==level),]
                mcolors <- c(mcolors,(as.character(refdf[match(sub_temp[,3],refdf[,1]),2])))
              }
              if(median_colored_as_points == FALSE){
                dat$colour <- rep(median_color,nrow(dat))
              }else{
                dat$colour <- mcolors
              }
              if(is.null(shape_val)){
                
                p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                    y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
                
                
                
              }else{
                p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                    y=middle, yend=middle), colour=dat$colour, size=size_median,inherit.aes = F)
                p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
                                  size = size_point,stroke=stroke_point,alpha=alpha_point,aes_string(x=x_val,y=y_val,col=col_val,shape=shape_val),inherit.aes = F)
              }
            }else{
              stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment constructing the figure manually",call.=TRUE)
              
            }
          }
          
        }


      }
  }else if (style == "mix"){
    #Mix style that paints the box black and the points and median of colour
      if(is.null(facet_formula)){
          if(col_val == x_val){
            ########################
            ########################
            ##Mix Style no facet###
            ###### With color ######
            ####col_val==x_val######
            ########################
            ########################
            myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
            p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
              geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
              scale_color_manual(values = rep("#414141",myrep)) + 
              geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                         size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
              scale_fill_manual(values = mpalette) 
            
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
            if(median_colored_as_points == FALSE){
              dat$colour <- rep(median_color,nrow(dat))
            }else{
              dat$colour <- mcolors
            }
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),colour=dat$colour,size=size_median)
            
            
           
          }else{
            ########################
            ########################
            ##Mix Style no facet###
            ###### With color ######
            ####col_val!=x_val######
            ########################
            ########################
            myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
            p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
              geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
              scale_color_manual(values = rep("#414141",myrep)) + 
              geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                         size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
              scale_fill_manual(values = mpalette)
            
            dat <- ggplot_build(p)$data[[1]]
            map_melted <- melt(acast(get(x_val)~get(col_val),data = Map,fun.aggregate = length,value.var = y_val,drop = T))
            colnames(map_melted)[1:2] <- c(x_val,col_val)
            map_melted <- map_melted[which(map_melted$value!=0),]
            map_melted <- map_melted[order(map_melted[,1]),]
            refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
            mcolors <- NULL
            for(level in levels(map_melted[,1])){
              sub_temp <- map_melted[which(map_melted[,1]==level),]
              mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
            }
            if(median_colored_as_points == FALSE){
              dat$colour <- rep(median_color,nrow(dat))
            }else{
              dat$colour <- mcolors
            }
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),colour=dat$colour,size=size_median)
            

          }
        }else{
          if(col_val != x_val){
            ########################
            ########################
            ##Mix Style with facet#
            ###### With color ######
            ####col_val!=x_val######
            ########################
            ########################
            if(lfacet == 1){
              myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = rep("#414141",myrep)) + 
                geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                           size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
                scale_fill_manual(values = mpalette)
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"+",as.character(col_val),"~",as.character(x_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1,2),variable.name = x_val)
              map_melted <- map_melted[which(map_melted$value!=0),]
              refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
              int_cols <- which(colnames(map_melted)!=col_val)
              map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]]),]
              combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],sep = "_")
              map_melted$Combined <- factor(combined,levels = unique(combined))
              dat <- cbind(dat,map_melted)
              mcolors <- NULL
              for(level in levels(map_melted$Combined)){
                sub_temp <- map_melted[which(map_melted$Combined==level),]
                mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
              }
              if(median_colored_as_points == FALSE){
                dat$colour <- rep(median_color,nrow(dat))
              }else{
                dat$colour <- mcolors
              }            
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              
              
            }else if (lfacet ==2){
              myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = rep("#414141",myrep)) + 
                geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                           size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
                scale_fill_manual(values = mpalette)
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"+",as.character(col_val),"~",as.character(x_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1,2,3),,variable.name = x_val)
              map_melted <- map_melted[which(map_melted$value!=0),]
              refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
              int_cols <- which(colnames(map_melted)!=col_val)
              map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]],map_melted[,int_cols[3]]),]
              combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],map_melted[,int_cols[3]],sep = "_")
              map_melted$Combined <- factor(combined,levels = unique(combined))
              dat <- cbind(dat,map_melted)
              mcolors <- NULL
              for(level in levels(map_melted$Combined)){
                sub_temp <- map_melted[which(map_melted$Combined==level),]
                mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,3],refdf[,1]),2])))
              }
              if(median_colored_as_points == FALSE){
                dat$colour <- rep(median_color,nrow(dat))
              }else{
                dat$colour <- mcolors
              }                
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              
              
            }else{
              stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment to construct the figure manually",call.=TRUE)
              
            }
          }else{
            ########################
            ########################
            ##Mix Style With facet##
            ###### With color ######
            ####col_val==x_val######
            ########################
            ########################
            if(lfacet == 1){
              myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = rep("#414141",myrep)) + 
                geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                           size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
                scale_fill_manual(values = mpalette)
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"~",as.character(col_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1),variable.name = x_val)
              map_melted <- map_melted[which(map_melted$value!=0),]
              refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
              int_cols <- c(1)
              map_melted <- map_melted[order(map_melted[,int_cols[1]]),]
              combined <- paste(map_melted[,int_cols[1]],sep = "_")
              map_melted$Combined <- factor(combined,levels = unique(combined))
              dat <- cbind(dat,map_melted)
              mcolors <- NULL
              for(level in levels(map_melted$Combined)){
                sub_temp <- map_melted[which(map_melted$Combined==level),]
                mcolors <- c(mcolors,rev(as.character(refdf[match(sub_temp[,2],refdf[,1]),2])))
              }
              if(median_colored_as_points == FALSE){
                dat$colour <- rep(median_color,nrow(dat))
              }else{
                dat$colour <- mcolors
              }            
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              
              
            }else if (lfacet ==2){
              myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = rep("#414141",myrep)) + 
                geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                           size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)  +
                scale_fill_manual(values = mpalette)
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"~",as.character(col_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1,2),,variable.name = x_val)
              map_melted <- map_melted[which(map_melted$value!=0),]
              refdf <- data.frame(col_val = levels(Map[,which(colnames(Map)==col_val)]),colors=mpalette)
              int_cols <- c(1,2)
              map_melted <- map_melted[order(map_melted[,int_cols[1]],map_melted[,int_cols[2]]),]
              combined <- paste(map_melted[,int_cols[1]],map_melted[,int_cols[2]],sep = "_")
              map_melted$Combined <- factor(combined,levels = unique(combined))
              dat <- cbind(dat,map_melted)
              mcolors <- NULL
              for(level in levels(map_melted$Combined)){
                sub_temp <- map_melted[which(map_melted$Combined==level),]
                mcolors <- c(mcolors,(as.character(refdf[match(sub_temp[,3],refdf[,1]),2])))
              }
              if(median_colored_as_points == FALSE){
                dat$colour <- rep(median_color,nrow(dat))
              }else{
                dat$colour <- mcolors
              }                
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              
              
            }else{
              stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment to construct the figure manually",call.=TRUE)
              
            }
          }
            

        }

      
  }else{
    stop("ERROR: Unrecognized style, try mix (default), full or open.",call.=TRUE)
    
    }
  #Add the theme style here
  p <- p +
    theme(axis.line = element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major.x =  element_blank(),
          panel.grid.major.y =element_line(colour = "#D9D9D9",size=size_lines_panel),
          panel.grid.minor.y = element_line(colour = "#D9D9D9",size=size_lines_panel),
          panel.border = element_rect(fill=NA,color =  "#414141",size = size_panel_border),
          axis.ticks = element_line(colour = "black",size = 2.5),
          axis.text.x = element_text(family = font_family,face = "plain",size =size_axis_text.x,colour="#414141",angle = 0),
          axis.text.y = element_text(family = font_family,face="plain",size=size_axis_text.y,colour="#414141"),
          axis.title.x = element_text(family = font_family,face="plain",size = size_axis_title.x,colour = "#414141"),
          axis.title.y = element_text(family = font_family,face="plain",size=size_axis_title.y,colour="#414141"),
          legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
          legend.title=element_text(size=size_title_text,
          family = font_family,face = "plain",colour = "#414141"),
          legend.key = element_blank(), 
          legend.text = element_text(size=size_legend_text,family = font_family,face = "plain",colour = "#414141"),
          legend.position ="right",strip.text = element_text(family = font_family,colour = "#414141",size = strip_text_size),
          strip.background = element_blank()) 
  return(p)
}

