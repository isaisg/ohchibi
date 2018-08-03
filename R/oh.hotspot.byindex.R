#' Given a list of numbers (genomic indices) finds indices that are physically closed in a genome.
#' Requires a threshold value of window to define distance between indices. Default 10
#'
#' 
#' @keywords hotspot,indices,index
#' @export
#' @examples
#' oh.hotspot.byindex()




oh.hotspot.byindex <- function(thres = 10,indices = NULL,num_hotspot = 0){
  i <- 1
  counter <- 1
  total <- length(indices)
  temp_hotspot <- NULL
  df_hotspots <- NULL
  while(counter < total){
    first <- indices[i]
    second <- indices[i+1]
    diff <- second - first
    counter <- counter+1
    i <- i+1
    #Evaluate if difference lower than threshold
    if(diff <= thres){
      temp_hotspot <- c(temp_hotspot,first,second)
      #Here evaluate if the next one is last recursive
      #The problem is that if there are only two 10,11
      #Even they are in a hotspot they wont form a hit because 
      #the call to evaluate temp_hotspot is in the next round
      if(counter >= total){
        num_hotspot <- num_hotspot+1
        tdf <- data.frame(index = unique(temp_hotspot),
                          hotspot = rep(paste0("Hotspot",num_hotspot),length(unique(temp_hotspot))))
        df_hotspots <- rbind(df_hotspots,tdf)
        temp_hotspot <-NULL
      }
    
    }else{
      #Evaluate if temp_hotspot is empty
      if(! is.null(temp_hotspot)){
        num_hotspot <- num_hotspot+1
        tdf <- data.frame(index = unique(temp_hotspot),
                          hotspot = rep(paste0("Hotspot",num_hotspot),length(unique(temp_hotspot))))
        df_hotspots <- rbind(df_hotspots,tdf)
        temp_hotspot <-NULL
      }
    }
  }
  #Evaluate if df_hostpost is empty
  #If this is the case fill with NA
  #This happens when there is not any overlap in a contig
  if(is.null(df_hotspots)){
    df_hotspots <- data.frame(index = NA,
               hotspot = NA)
  }
  return(df_hotspots)
}
