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
                        alpha_point=0.5,stroke_point=0.5,size_boxplot=0.5,size_median=2){
  if(is.null(mpalette)){
    mpalette <- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666")
    mpalette <- mpalette[1:length(levels(Map[,which(colnames(Map)==col_val)]))]
    
  }
  #Shape value should only make sense in the open style
  if(! is.null(shape_val) & style!="open"){
    stop("ERROR: the shape_val plotting is implement only in the open style. please change style=open",call.=TRUE)
    
  }
  if(style == "full"){
    if(is.null(col_val)){
      stop("ERROR: Need to provide a col_val when using the full style",call.=TRUE)
    }else{
      #Check if facetting
      if(is.null(facet_formula)){
        #Not facetting full style
        #Full style with medians of different color
        if(median_colored_as_points == FALSE){
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
          #Not shape variable used
          dat <- ggplot_build(p)$data[[1]]
          p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                              y=middle, yend=middle), colour=median_color, size=size_median,inherit.aes = F)
          p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                            jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
          
          myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
          x_interval <- 1:myrep
          limit_x <- length(x_interval)-1
          x_interval <- x_interval[1:limit_x]+0.5
          p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
          
         
        }else{
          #Not facetting
          #Style with medians painted the same color
          #This style is not recommended. Nevertheless we code it just for the sanity of my mind.
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
                                              y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
          p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                            jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
          
          myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
          x_interval <- 1:myrep
          limit_x <- length(x_interval)-1
          x_interval <- x_interval[1:limit_x]+0.5
          p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
          
        }
      }else{
        #Facetting with full style
        facet_formula <- gsub(pattern = " ",replacement = "",x = facet_formula)
        lfacet <- length(unlist(strsplit(x = facet_formula,split = "\\+")))
        facet_vals <- unlist(strsplit(x = facet_formula,split = "\\+"))
        facet_formula <- as.formula(paste("~", facet_formula))
        if(lfacet == 1){
          #Facetting with only one 
          if(median_colored_as_points == FALSE){
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
            dat$colour <- rep(median_color,nrow(dat))
            
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                              jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
            
            #Draw the automatic vline
            #Determine the levels per facet_val
            num_facet <- length(levels(Map[,which(colnames(Map)==facet_vals[1])]))
            num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
            myrep <- num_facet*num_x
            iter <- myrep/num_x
            tot_interval <- 1:myrep
            #Pick the number of groups 
            flag <- 1
            i <- 1
            x_interval <- NULL
            while (flag ==1){
              j <- (i + iter)-1
              mnums <- tot_interval[i:j]
              for(k in 1:(length(mnums)-1)){
                x_interval <- c(x_interval,mnums[k] + 0.5)
              }
              i <- i + iter
              if(i >= myrep){
                flag = 0
              }
            }
            p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
          }else{
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
            dat$colour <- mcolors
            
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                              jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
            
            #Draw the automatic vline
            #Determine the levels per facet_val
            num_facet <- length(levels(Map[,which(colnames(Map)==facet_vals[1])]))
            num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
            myrep <- num_facet*num_x
            iter <- myrep/num_x
            tot_interval <- 1:myrep
            #Pick the number of groups 
            flag <- 1
            i <- 1
            x_interval <- NULL
            while (flag ==1){
              j <- (i + iter)-1
              mnums <- tot_interval[i:j]
              for(k in 1:(length(mnums)-1)){
                x_interval <- c(x_interval,mnums[k] + 0.5)
              }
              i <- i + iter
              if(i >= myrep){
                flag = 0
              }
            }
            p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
          }
        
        }else if (lfacet ==2){
          if(median_colored_as_points == FALSE){
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
            dat$colour <- rep(median_color,nrow(dat))
            
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                              jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
            
            #Draw the automatic vline
            mlevs <- paste(map_melted[,which(colnames(map_melted)==facet_vals[1])],
                           map_melted[,which(colnames(map_melted)==facet_vals[2])],sep = "_")
            num_facet <- length(unique(mlevs))
            num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
            myrep <- num_facet*num_x
            iter <- myrep/num_x
            tot_interval <- 1:myrep
            #Pick the number of groups 
            flag <- 1
            j <- 1
            x_interval <- NULL
            for(i in 1:(iter-1)){
              mnums <- tot_interval[j:(j+num_x-1)]
              for(k in 1:(length(mnums)-1)){
                x_interval <- c(x_interval,mnums[k] + 0.5)
              }
              j <- j + num_x
            }
            p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
          }else{
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
            dat$colour <- mcolors
            
            p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
            p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                              jitter.width = 0.1), size = size_point,shape = mypch_point,col="#414141",stroke=stroke_point,alpha=alpha_point,inherit.aes = T)
            
            mlevs <- paste(map_melted[,which(colnames(map_melted)==facet_vals[1])],
                           map_melted[,which(colnames(map_melted)==facet_vals[2])],sep = "_")
            num_facet <- length(unique(mlevs))
            num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
            myrep <- num_facet*num_x
            iter <- myrep/num_x
            tot_interval <- 1:myrep
            #Pick the number of groups 
            flag <- 1
            j <- 1
            x_interval <- NULL
            for(i in 1:(iter-1)){
              mnums <- tot_interval[j:(j+num_x-1)]
              for(k in 1:(length(mnums)-1)){
                x_interval <- c(x_interval,mnums[k] + 0.5)
              }
              j <- j + num_x
            }
            p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
            
          }
         
        }else{
          stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment constructing the figure manually",call.=TRUE)
          
        }
      }
   
 
    }
  }else if(style == "open"){
      #This is the black style with red medians. Great when there are a lot of points such as RNA-Seq
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
          cat(x_interval)
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
        #Check if facetting is requested
        if(is.null(facet_formula)){
          if(median_colored_as_points == FALSE){
            #Colored using the median color
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
              myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
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
          }else{
            #Colored according to the same color of the box
            p <- ggplot(Map, aes_string(x = x_val, y = y_val, col = col_val,  fill = col_val)) + 
              geom_boxplot(fill = NA, outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
              geom_point(aes_string(fill=col_val),position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
                         size = size_point, shape = mypch_point, col = color_boxplot,stroke=stroke_point,alpha=alpha_point)+
              scale_color_manual(values = mpalette)+ scale_fill_manual(values = mpalette) +
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
                                                  y=middle, yend=middle),colour=dat$colour,size=size_median,inherit.aes = F)
              
              myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
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
          
        }else{
          #Facetting 
          facet_formula <- gsub(pattern = " ",replacement = "",x = facet_formula)
          lfacet <- length(unlist(strsplit(x = facet_formula,split = "\\+")))
          facet_vals <- unlist(strsplit(x = facet_formula,split = "\\+"))
          facet_formula <- as.formula(paste("~", facet_formula))
          if(lfacet == 1){
            #Implement as well two options to color the median
            if(median_colored_as_points == FALSE){
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = mpalette)+
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
                      strip.background = element_blank()) 
              
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
              dat$colour <- rep(median_color,nrow(dat))
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              #Draw the automatic vline
              #Determine the levels per facet_val
              num_facet <- length(levels(Map[,which(colnames(Map)==facet_vals[1])]))
              num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
              myrep <- num_facet*num_x
              iter <- myrep/num_x
              tot_interval <- 1:myrep
              #Pick the number of groups 
              flag <- 1
              i <- 1
              x_interval <- NULL
              while (flag ==1){
                j <- (i + iter)-1
                mnums <- tot_interval[i:j]
                for(k in 1:(length(mnums)-1)){
                  x_interval <- c(x_interval,mnums[k] + 0.5)
                }
                i <- i + iter
                if(i >= myrep){
                  flag = 0
                }
              }
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
            }else{
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = mpalette)+
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
                      strip.background = element_blank()) 
              
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
              dat$colour <- mcolors
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              
              #Draw the automatic vline
              #Determine the levels per facet_val
              num_facet <- length(levels(Map[,which(colnames(Map)==facet_vals[1])]))
              num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
              myrep <- num_facet*num_x
              iter <- myrep/num_x
              tot_interval <- 1:myrep
              #Pick the number of groups 
              flag <- 1
              i <- 1
              x_interval <- NULL
              while (flag ==1){
                j <- (i + iter)-1
                mnums <- tot_interval[i:j]
                for(k in 1:(length(mnums)-1)){
                  x_interval <- c(x_interval,mnums[k] + 0.5)
                }
                i <- i + iter
                if(i >= myrep){
                  flag = 0
                }
              }
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
            }
            
          }else if (lfacet ==2){
            if(median_colored_as_points == FALSE){
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = mpalette)+
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
                      strip.background = element_blank()) 
              
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
              dat$colour <- rep(median_color,nrow(dat))
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              #Draw the automatic vline
              mlevs <- paste(map_melted[,which(colnames(map_melted)==facet_vals[1])],
                             map_melted[,which(colnames(map_melted)==facet_vals[2])],sep = "_")
              num_facet <- length(unique(mlevs))
              num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
              myrep <- num_facet*num_x
              iter <- myrep/num_x
              tot_interval <- 1:myrep
              #Pick the number of groups 
              flag <- 1
              j <- 1
              x_interval <- NULL
              for(i in 1:(iter-1)){
                mnums <- tot_interval[j:(j+num_x-1)]
                for(k in 1:(length(mnums)-1)){
                  x_interval <- c(x_interval,mnums[k] + 0.5)
                }
                j <- j + num_x
              }
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
            }else{
              p <- ggplot(Map, aes_string(x = x_val, y = y_val)) + 
                geom_boxplot(aes_string(color=col_val),outlier.colour = NA, position = position_dodge(width = 0.9), size=size_boxplot) +
                scale_color_manual(values = mpalette)+
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
                      strip.background = element_blank()) 
              
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
              dat$colour <- mcolors
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              mlevs <- paste(map_melted[,which(colnames(map_melted)==facet_vals[1])],
                             map_melted[,which(colnames(map_melted)==facet_vals[2])],sep = "_")
              num_facet <- length(unique(mlevs))
              num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
              myrep <- num_facet*num_x
              iter <- myrep/num_x
              tot_interval <- 1:myrep
              #Pick the number of groups 
              flag <- 1
              j <- 1
              x_interval <- NULL
              for(i in 1:(iter-1)){
                mnums <- tot_interval[j:(j+num_x-1)]
                for(k in 1:(length(mnums)-1)){
                  x_interval <- c(x_interval,mnums[k] + 0.5)
                }
                j <- j + num_x
              }
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
            }
          }
        }

      }
  }else if (style == "mix"){
    #Mix style that paints the box black and the points and median of colour
    if(is.null(shape_val)){
      if(is.null(facet_formula)){
          if(col_val == x_val){
            if(median_colored_as_points == FALSE){
              #Style of mix where the x_val is equal to col_val
              #The median is colored according to the input of the user
              myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
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
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                  y=middle, yend=middle), colour=median_color, size=size_median,inherit.aes = F)
              x_interval <- 1:myrep
              limit_x <- length(x_interval)-1
              x_interval <- x_interval[1:limit_x]+0.5
              
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
            }else{
              #The median is colored according to the points
              myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
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
            }
           
          }else{
            myrep <- length(unique(Map[,which(colnames(Map)==col_val)]))
            if(median_colored_as_points == FALSE){
              #Color the median according to the input of the user
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
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                  y=middle, yend=middle), colour=median_color, size=size_median,inherit.aes = F)
              myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
              x_interval <- 1:myrep
              limit_x <- length(x_interval)-1
              x_interval <- x_interval[1:limit_x]+0.5
              
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
            }else{
              #Color the median according to the colors of the points
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
              dat$colour <- mcolors
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),colour=dat$colour,size=size_median)
              
              myrep <- length(unique(Map[,which(colnames(Map)==x_val)]))
              x_interval <- 1:myrep
              limit_x <- length(x_interval)-1
              x_interval <- x_interval[1:limit_x]+0.5
              
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
            }

          }
        }else{
          #Facetting 
          facet_formula <- gsub(pattern = " ",replacement = "",x = facet_formula)
          lfacet <- length(unlist(strsplit(x = facet_formula,split = "\\+")))
          facet_vals <- unlist(strsplit(x = facet_formula,split = "\\+"))
          facet_formula <- as.formula(paste("~", facet_formula))
          if(lfacet == 1){
            #Implement as well two options to color the median
            if(median_colored_as_points == FALSE){
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
                      strip.background = element_blank()) 
              
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
              dat$colour <- rep(median_color,nrow(dat))
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              #Draw the automatic vline
              #Determine the levels per facet_val
              num_facet <- length(levels(Map[,which(colnames(Map)==facet_vals[1])]))
              num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
              myrep <- num_facet*num_x
              iter <- myrep/num_x
              tot_interval <- 1:myrep
              #Pick the number of groups 
              flag <- 1
              i <- 1
              x_interval <- NULL
              while (flag ==1){
                j <- (i + iter)-1
                mnums <- tot_interval[i:j]
                for(k in 1:(length(mnums)-1)){
                  x_interval <- c(x_interval,mnums[k] + 0.5)
                }
                i <- i + iter
                if(i >= myrep){
                  flag = 0
                }
              }
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
            }else{
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
                      strip.background = element_blank()) 
              
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
              dat$colour <- mcolors
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              
              #Draw the automatic vline
              #Determine the levels per facet_val
              num_facet <- length(levels(Map[,which(colnames(Map)==facet_vals[1])]))
              num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
              myrep <- num_facet*num_x
              iter <- myrep/num_x
              tot_interval <- 1:myrep
              #Pick the number of groups 
              flag <- 1
              i <- 1
              x_interval <- NULL
              while (flag ==1){
                j <- (i + iter)-1
                mnums <- tot_interval[i:j]
                for(k in 1:(length(mnums)-1)){
                  x_interval <- c(x_interval,mnums[k] + 0.5)
                }
                i <- i + iter
                if(i >= myrep){
                  flag = 0
                }
              }
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
              }
            
          }else if (lfacet ==2){
            if(median_colored_as_points == FALSE){
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
                      strip.background = element_blank()) 
              
              p <- p + facet_grid(facets = facet_formula,scales = "free",space = "free")
              red_formula <- grep(pattern = "~",x = as.character(facet_formula),invert = T,value = T)
              red_formula <- as.formula(paste(red_formula,"+",as.character(col_val),"~",as.character(x_val),sep = " "))
              dat <- ggplot_build(p)$data[[1]]
              map_melted <- melt(dcast(formula = red_formula,data=Map,fun.aggregate = length,value.var = y_val,drop = T),
                                 id.vars = c(1,2,3),variable.name = x_val)
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
              dat$colour <- rep(median_color,nrow(dat))
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              #Draw the automatic vline
              mlevs <- paste(map_melted[,which(colnames(map_melted)==facet_vals[1])],
                                     map_melted[,which(colnames(map_melted)==facet_vals[2])],sep = "_")
              num_facet <- length(unique(mlevs))
              num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
              myrep <- num_facet*num_x
              iter <- myrep/num_x
              tot_interval <- 1:myrep
              #Pick the number of groups 
              flag <- 1
              j <- 1
              x_interval <- NULL
              for(i in 1:(iter-1)){
                mnums <- tot_interval[j:(j+num_x-1)]
                for(k in 1:(length(mnums)-1)){
                  x_interval <- c(x_interval,mnums[k] + 0.5)
                }
                j <- j + num_x
              }
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
            }else{
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
                      strip.background = element_blank() ) 
              
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
              dat$colour <- mcolors
              
              p <- p + geom_segment(data=dat, aes(x=xmin, xend=xmax,
                                                  y=middle, yend=middle),color = dat$colour,size=size_median,inherit.aes = F)
              mlevs <- paste(map_melted[,which(colnames(map_melted)==facet_vals[1])],
                             map_melted[,which(colnames(map_melted)==facet_vals[2])],sep = "_")
              num_facet <- length(unique(mlevs))
              num_x <- length(levels(Map[,which(colnames(Map)==x_val)]))
              myrep <- num_facet*num_x
              iter <- myrep/num_x
              tot_interval <- 1:myrep
              #Pick the number of groups 
              flag <- 1
              j <- 1
              x_interval <- NULL
              for(i in 1:(iter-1)){
                mnums <- tot_interval[j:(j+num_x-1)]
                for(k in 1:(length(mnums)-1)){
                  x_interval <- c(x_interval,mnums[k] + 0.5)
                }
                j <- j + num_x
              }
              p <- p + geom_vline(xintercept = x_interval,color="#D9D9D9",size=0.3)
              
            }
         
          }else{
            stop("ERROR: More than 2 variables in facetting  is not implemented yet. I recomment to construct the figure manually",call.=TRUE)
            
          }
        }
      }else{
        stop("ERROR: Shape_val is not implemented in the mix style. Use the open style to better plot shape_val",call.=TRUE)
        
      }
      
  }else{
    stop("ERROR: Unrecognized style, try mix (default), full or open.",call.=TRUE)
    
    }

  return(p)
}

