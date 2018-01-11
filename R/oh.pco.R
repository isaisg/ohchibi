#' Performs Principal Coordinate Analysis based on cmdscale command
#'
#' Two fundamental structures needed. The abundance matrix (Tab) and metadata (Map)
#' @keywords pco
#' @export
#' @examples
#' oh.pco()

oh.pco<-function(Tab=Tab,Map=Map,ndim=3,eig=T,distfun=distfun,id_var="Id"){
  Tab_dist <- distfun(Tab)
  res <- cmdscale(Tab_dist, k = ndim, eig = eig)
  perc_var <- (100*res$eig / sum(res$eig[res$eig > 0 ]))
  perc_var<-perc_var[1:ndim]
  points<-res$points
  #Define name for the columns in the points object
  newcols<-NULL
  for (i in 1:ndim){
    temp<-paste("PCo",i,sep="")
    newcols<-c(newcols,temp)
  }
  colnames(points)<-newcols
  names(perc_var)<-newcols
  perc_var<-round(perc_var,digits = 2)
  points<-points[match(Map[,which(colnames(Map)==id_var)],rownames(points)),]
  Map_pco <-cbind(Map,points)
  toret=list(Map_pco = Map_pco, variance_explained=perc_var)
  return(toret)
}

