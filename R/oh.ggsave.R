#' Saves a ggplot object in a publication ready high resolution formar
#'
#' 
#' @keywords ggsave
#' @export
#' @examples
#' oh.ggsave()


oh.ggsave.svg<-function(ggobject=p,outname="",outdir="figures/",width=20,height=15,device="svg",dpi=600){
  dir.create(outdir, showWarnings = FALSE)
  myfilename<-paste(outdir,outname,sep="/")
  ggsave(filename = myfilename,plot = ggobject,width =width,height = height,device = device,dpi = dpi)
}

