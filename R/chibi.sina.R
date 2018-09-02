#' Creates a sina plot ussing ggplot2 using variables in a metadata (Map) structure
#'
#' The sina plot  can represent up to two variables in a single plot
#' @keywords sina
#' @export
#' @examples
#' chibi.sina()

chibi.sina <- function(Map=Map,x_val=NULL,y_val=NULL,col_val=NULL,
	mpalette = NULL,facet_formula=NULL,
  df_stats = NULL,mean_var = NULL,ymin_var = NULL, ymax_var = NULL,
	show_points = TRUE , color_points = FALSE, color_bar = TRUE,
  bar_color = "#414141",points_color = "#414141",size_point=10,
  alpha_point=0.3,stroke_point=0.5,size_bar=2,width_bar = 0.2,
  size_axis_text.x=20,size_axis_text.y=20,
  size_axis_title.x=30,size_axis_title.y=30,
  size_legend_text=20,size_title_text = 30,strip_text_size=20,legend_proportion_size=2,
  size_lines_panel = 0.3,size_panel_border = 1,font_family = "Arial"){
  if(is.null(mpalette)){
    mpalette <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
      "#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928")
    #mpalette <- mpalette[1:length(levels(Map[,which(colnames(Map)==col_val)]))]
    
  }
  #Evaluate if facet is imposed
  if(is.null(facet_formula)){
  #No facet
  #Compute the statistics for each level on x_val based on y_val
  #Evaluate if no dfstats passed
  if(is.null(df_stats)){
   f1 <- formula(paste0(y_val,"~",x_val))
   tdf <- aggregate(f1,Map,
          FUN = function(x) c(mmean = mean(x),
                mmedian = median(x), msd = sd(x))
          )
   dfs <- data.frame(Id = tdf[[1]],Mean = tdf[[2]][,1],
             Median = tdf[[2]][,2], SD = tdf[[2]][,3])
   colnames(dfs)[1] <- x_val
   dfs$ymax <- dfs$Mean + dfs$SD
   dfs$ymin <- dfs$Mean - dfs$SD
  }else{
    #Check the names of the xval
    if(is.null(mean_var) | is.null(ymin_var) | is.null(ymax_var)){
      stop("ERROR: When passing df_stats you should give me the names of the variables with the mean,ymin and ymax",call.=TRUE)
    }
    #Check if exists the x_val in the passed stats data.frame
    if(length(which(colnames(df_stats) == x_val)) == 0){
            stop("ERROR: The x_val you chose is not present as column in the df_stats you passed. Please add it",call.=TRUE)

    }
    dfs <- df_stats
    colnames(dfs)[which(colnames(dfs) == mean_var)] <- "Mean"
    colnames(dfs)[which(colnames(dfs) == ymin_var)] <- "ymin"
    colnames(dfs)[which(colnames(dfs) == ymax_var)] <- "ymax"
    #Adjust the levels to be consistent
    niv <- levels(Map[,which(colnames(Map) == x_val)])
    dfs[,which(colnames(dfs)==x_val)] <- factor(dfs[,which(colnames(dfs) == x_val)],levels = niv)
  }

    #Here evaluate the different styles
    if(show_points == FALSE){
      if (color_bar == TRUE){
        p <- ggplot(data = Map,aes_string(x = x_val, y = y_val, color = x_val)) +
		      geom_errorbar(aes(y = Mean, ymin = ymin , ymax= ymax), 
          data = dfs, width = width_bar,size = size_bar) + 
          geom_point(aes(y = Mean), size = size_point, data = dfs) 
	      p <- p + scale_color_manual(values = mpalette)
      }else{
        p <- ggplot(data = Map,aes_string(x = x_val, y = y_val)) +
               geom_errorbar(aes(y = Mean, ymin = ymin , ymax= ymax), 
               data = dfs,color = bar_color, width = width_bar,size = size_bar) + 
               geom_point(aes(y = Mean),color = bar_color, size = size_point, data = dfs) 
	   }
    }else{
      if(color_points == FALSE & color_bar == TRUE){
        p <- ggplot(data = Map,aes_string(x = x_val ,y = y_val, color = x_val)) +
           geom_sina(size = size_point,shape = 21,alpha = alpha_point, color = points_color) +
           geom_point(aes(y = Mean), size = size_point, data = dfs) + 
            geom_errorbar(aes(y = Mean, ymin = ymin , ymax=ymax), 
                  data = dfs, width = width_bar,size = size_bar)	
        p <- p + scale_color_manual(values = mpalette)

  	  }else if(color_points == TRUE & color_bar == TRUE){
  		  p <- ggplot(data = Map,aes_string(x = x_val ,y = y_val, color = x_val)) +
  			geom_sina(size = size_point,shape = 21,alpha = alpha_point) +
  			  geom_point(aes(y = Mean), size = size_point, data = dfs) + 
  			  geom_errorbar(aes(y = Mean, ymin = ymin , ymax= ymax), 
  	                data = dfs, width = width_bar,size = size_bar)
        p <- p + scale_color_manual(values = mpalette)
		
  	  }else if (color_points == TRUE & color_bar == FALSE){
    		p <- ggplot(data = Map,aes_string(x = x_val ,y = y_val, color = x_val)) +
    		geom_sina(size = size_point,shape = 21,alpha = alpha_point) +
    		geom_point(aes(y = Mean), size = size_point, data = dfs,color = bar_color) + 
    		geom_errorbar(aes(y = Mean, ymin = ymin, ymax= ymax), 
                    data = dfs, width = width_bar,size = size_bar,color = bar_color) 
    	   p <- p + scale_color_manual(values = mpalette)

      }else{
    		p <- ggplot(data = Map,aes_string(x = x_val ,y = y_val)) +
    		geom_sina(size = size_point,shape = 21,alpha = alpha_point,color = points_color) +
    		geom_point(aes(y = Mean), size = size_point, data = dfs,color = "#414141") + 
    		geom_errorbar(aes(y = Mean, ymin = ymin, ymax= ymax), 
                    data = dfs, width = width_bar,size = size_bar,color = "#414141") 
  
      } 
    }
  }else{
    #Facet 
    facet_formula <- gsub(pattern = " ",replacement = "",x = facet_formula)
    lfacet <- length(unlist(strsplit(x = facet_formula,split = "\\+")))
    facet_vals <- unlist(strsplit(x = facet_formula,split = "\\+"))
    facet_formula <- as.formula(paste("~", facet_formula))
    if(lfacet == 1){
      #Option of only one facette
      #Determine the levels and order of that factor
      mlevels <- levels(Map[,which(colnames(Map) == facet_vals[1])])
      if(is.null(df_stats)){
        f1 <- formula(paste0(y_val,"~",facet_vals[1], "+", x_val))
        tdf <- aggregate(f1,Map,
                     FUN = function(x) c(mmean = mean(x),
                                         mmedian = median(x), msd = sd(x))
                )
        tdf[,1] <- factor(tdf[,1],levels = mlevels)
        #Construct data frame of positions 
        dfs <- data.frame(Id1 = tdf[[1]],Id2 = tdf[[2]],Mean = tdf[[3]][,1],
                          Median = tdf[[3]][,2], SD = tdf[[3]][,3])
        colnames(dfs)[1:2] <- c(facet_vals[1],x_val)
        dfs$ymax <- dfs$Mean + dfs$SD
        dfs$ymin <- dfs$Mean - dfs$SD
      }else{
        if(is.null(mean_var) | is.null(ymin_var) | is.null(ymax_var)){
          stop("ERROR: When passing df_stats you should give me the names of the variables with the mean,ymin and ymax",call.=TRUE)
        }
        #Check if exists the x_val in the passed stats data.frame
        if(length(which(colnames(df_stats) == x_val)) == 0){
          stop("ERROR: The x_val you chose is not present as column in the df_stats you passed. Please add it",call.=TRUE)

        }
        #Check if facet vals exist in the structured passed
        if(length(which(colnames(df_stats) == facet_vals[1])) == 0){
          stop("ERROR: The facet_val you chose is not present as column in the df_stats you passed. Please add it",call.=TRUE)
        }
        dfs <- df_stats
        colnames(dfs)[which(colnames(dfs) == mean_var)] <- "Mean"
        colnames(dfs)[which(colnames(dfs) == ymin_var)] <- "ymin"
        colnames(dfs)[which(colnames(dfs) == ymax_var)] <- "ymax"
        #Adjust the levels to be consistent
        niv <- levels(Map[,which(colnames(Map) == x_val)])
        dfs[,which(colnames(dfs)==x_val)] <- factor(dfs[,which(colnames(dfs) == x_val)],levels = niv)
        niv <- levels(Map[,which(colnames(Map) == facet_vals[1])])
        dfs[,which(colnames(dfs)==facet_vals[1])] <- factor(dfs[,which(colnames(dfs) == facet_vals[1])],levels = niv)

      }
      #Give all the options to construct the sina based on different aesthetics
        if(show_points == FALSE){
          if (color_bar == TRUE){
            p <- ggplot(data = Map,aes_string(x = x_val, y = y_val, color = x_val)) +
              facet_grid(facets = facet_formula,scales = "free",space = "free") + 
              geom_errorbar(aes(y = Mean, ymin = ymin , ymax= ymax), 
              data = dfs, width = width_bar,size = size_bar) + 
              geom_point(aes(y = Mean), size = size_point, data = dfs) 
            p <- p + scale_color_manual(values = mpalette)
          }else{
            p <- ggplot(data = Map,aes_string(x = x_val, y = y_val)) +
                   facet_grid(facets = facet_formula,scales = "free",space = "free") + 
                   geom_errorbar(aes(y = Mean, ymin = ymin, ymax= ymax), 
                   data = dfs,color = bar_color, width = width_bar,size = size_bar) + 
                   geom_point(aes(y = Mean),color = bar_color, size = size_point, data = dfs) 
         }
      }else{
        if(color_points == FALSE & color_bar == TRUE){
          p <- ggplot(data = Map,aes_string(x = x_val ,y = y_val, color = x_val)) +
             facet_grid(facets = facet_formula,scales = "free",space = "free") + 
             geom_sina(size = size_point,shape = 21,alpha = alpha_point, color = points_color) +
             geom_point(aes(y = Mean), size = size_point, data = dfs) + 
              geom_errorbar(aes(y = Mean, ymin = ymin , ymax= ymax), 
                    data = dfs, width = width_bar,size = size_bar)  
          p <- p + scale_color_manual(values = mpalette)

        }else if(color_points == TRUE & color_bar == TRUE){
          p <- ggplot(data = Map,aes_string(x = x_val ,y = y_val, color = x_val)) +
          facet_grid(facets = facet_formula,scales = "free",space = "free") + 
          geom_sina(size = size_point,shape = 21,alpha = alpha_point) +
            geom_point(aes(y = Mean), size = size_point, data = dfs) + 
            geom_errorbar(aes(y = Mean, ymin = ymin , ymax= ymax), 
                      data = dfs, width = width_bar,size = size_bar)
          p <- p + scale_color_manual(values = mpalette)
      
        }else if (color_points == TRUE & color_bar == FALSE){
          p <- ggplot(data = Map,aes_string(x = x_val ,y = y_val, color = x_val)) +
          facet_grid(facets = facet_formula,scales = "free",space = "free") + 
          geom_sina(size = size_point,shape = 21,alpha = alpha_point) +
          geom_point(aes(y = Mean), size = size_point, data = dfs,color = bar_color) + 
          geom_errorbar(aes(y = Mean, ymin = ymin , ymax= ymax), 
                      data = dfs, width = width_bar,size = size_bar,color = bar_color) 
           p <- p + scale_color_manual(values = mpalette)

        }else{
          p <- ggplot(data = Map,aes_string(x = x_val ,y = y_val)) +
          facet_grid(facets = facet_formula,scales = "free",space = "free") + 
          geom_sina(size = size_point,shape = 21,alpha = alpha_point,color = points_color) +
          geom_point(aes(y = Mean), size = size_point, data = dfs,color = "#414141") + 
          geom_errorbar(aes(y = Mean, ymin = ymin , ymax= ymax), 
                      data = dfs, width = width_bar,size = size_bar,color = "#414141") 
    
        } 
      }
    }

  }
  #Add theme to the plot
  p <- p +  theme(axis.line = element_blank(),
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



