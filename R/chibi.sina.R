#' Creates a sina plot ussing ggplot2 using variables in a metadata (Map) structure
#'
#' The sina plot  can represent up to two variables in a single plot
#' @keywords sina
#' @export
#' @examples
#' chibi.sina()

chibi.sina <- function(Map=Map,x_val=NULL,y_val=NULL,col_val=NULL,
	mpalette=NULL,facet_formula=NULL,
	show_points = TRUE , color_points = FALSE, color_bar = TRUE,
  bar_color = "#414141",points_color = "#414141",size_point=10,
  alpha_point=0.3,stroke_point=0.5,size_bar=2,width_bar = 0.2,
  size_axis_text.x=20,size_axis_text.y=20,
  size_axis_title.x=30,size_axis_title.y=30,
  size_legend_text=20,strip_text_size=20,legend_proportion_size=2,
  size_lines_panel = 0.3,size_panel_border = 1){
  if(is.null(mpalette)){
    mpalette <- RColorBrewer::brewer.pal(n = 12,name = "Paired")
    mpalette <- mpalette[1:length(levels(Map[,which(colnames(Map)==col_val)]))]
    
  }
  #Evaluate if facet is imposed
  if(is.null(facet_formula)){
  #No facet
  #Compute the statistics for each level on x_val based on y_val
   f1 <- formula(paste0(y_val,"~",x_val))
   tdf <- aggregate(f1,Map,
          FUN = function(x) c(mmean = mean(x),
                mmedian = median(x), msd = sd(x))
          )
   dfs <- data.frame(Id = tdf[[1]],Mean = tdf$MainRoot[,1],
             Median = tdf$MainRoot[,2], SD = tdf$MainRoot[,3])
   colnames(dfs)[1] <- x_val
    #Here evaluate the different styles
    if(show_points == FALSE){
      if (color_bar == TRUE){
        p <- ggplot(data = Map,aes_string(x = x_val, y = y_val, color = x_val)) +
		      geom_errorbar(aes(y = Mean, ymin = Mean - SD , ymax= Mean + SD), 
          data = dfs, width = width_bar,size = size_bar) + 
          geom_point(aes(y = Mean), size = size_point, data = dfs) 
	      p <- p + scale_color_manual(values = mpalette)
      }else{
        p <- ggplot(data = Map,aes_string(x = x_val, y = y_val)) +
               geom_errorbar(aes(y = Mean, ymin = Mean - SD , ymax= Mean + SD), 
               data = dfs,color = bar_color, width = width_bar,size = size_bar) + 
               geom_point(aes(y = Mean),color = bar_color, size = size_point, data = dfs) 
	   }
    }else{
      if(color_points == FALSE & color_bar == TRUE){
        p <- ggplot(data = df,aes_string(x = x_val ,y = y_val, color = x_val)) +
           geom_sina(size = size_point,shape = 21,alpha = alpha_point, color = points_color) +
           geom_point(aes(y = Mean), size = size_point, data = dfs) + 
            geom_errorbar(aes(y = Mean, ymin = Mean - SD , ymax= Mean + SD), 
                  data = dfs, width = width_bar,size = size_bar)	
        p <- p + scale_color_manual(values = mpalette)

  	  }else if(color_points == TRUE & color_bar == TRUE){
  		  p <- ggplot(data = df,aes_string(x = x_val ,y = y_val, color = x_val)) +
  			geom_sina(size = size_point,shape = 21,alpha = alpha_point) +
  			  geom_point(aes(y = Mean), size = size_point, data = dfs) + 
  			  geom_errorbar(aes(y = Mean, ymin = Mean - SD , ymax= Mean + SD), 
  	                data = dfs, width = width_bar,size = size_bar)
        p <- p + scale_color_manual(values = mpalette)
		
  	  }else if (color_points == TRUE & color_bar == FALSE){
    		p <- ggplot(data = df,aes_string(x = x_val ,y = y_val, color = x_val)) +
    		geom_sina(size = size_point,shape = 21,alpha = alpha_point) +
    		geom_point(aes(y = Mean), size = size_point, data = dfs,color = bar_color) + 
    		geom_errorbar(aes(y = Mean, ymin = Mean - SD , ymax= Mean + SD), 
                    data = dfs, width = width_bar,size = size_bar,color = bar_color) 
    	   p <- p + scale_color_manual(values = mpalette)

      }else{
    		p <- ggplot(data = df,aes_string(x = x_val ,y = y_val)) +
    		geom_sina(size = size_point,shape = 21,alpha = alpha_point,color = points_color) +
    		geom_point(aes(y = Mean), size = size_point, data = dfs,color = "#414141") + 
    		geom_errorbar(aes(y = Mean, ymin = Mean - SD , ymax= Mean + SD), 
                    data = dfs, width = width_bar,size = size_bar,color = "#414141") 
  
      } 
    }
  }else{
      #Facet 
  }
  #Add theme to the plot
  p <- p +  theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x =  element_blank(),
        panel.grid.major.y =element_line(colour = "#D9D9D9",size=size_lines_panel),
        panel.grid.minor.y = element_line(colour = "#D9D9D9",size=size_lines_panel),
        panel.border = element_rect(fill=NA,color =  "#414141",size = size_panel_border),
        axis.ticks = element_line(colour = "black",size = 2.5),
        axis.text.x = element_text(family = "AvantGarde",face = "plain",size =size_axis_text.x,colour="#414141",angle = 0),
        axis.text.y = element_text(family = "AvantGarde",face="plain",size=size_axis_text.y,colour="#414141"),
        axis.title.x = element_text(family = "AvantGarde",face="plain",size = size_axis_title.x,colour = "#414141"),
        axis.title.y = element_text(family = "AvantGarde",face="plain",size=size_axis_title.y,colour="#414141"),
        legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
        legend.title=element_blank(),legend.key = element_blank(), 
        legend.text = element_text(size=size_legend_text,family = "AvantGarde",face = "plain",colour = "#414141"),
        legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = strip_text_size),
        strip.background = element_blank()) 
  return(p)
}



