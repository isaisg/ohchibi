#' Plots results from oh.cap
#'
#' Uses the structure return from oh.cap to automatically plot a scatterplot colored by variables from the Map structured used to compute oh.cap
#' @keywords cap
#' @export
#' @examples
#' chibi.cap()


chibi.cap<-function(list_ohpco=NULL,col_val=NULL,comp_a=NULL,comp_b=NULL,mypch=22,size=25,alpha=0.7,stroke=1.5){
  percvar<-list_ohpco$variance_explained_axis
  Map_cap<-list_ohpco$Map_cap
  vartot<-list_ohpco$total_var
  pval<-list_ohpco$pval_model
  p <- ggplot(data = Map_cap,aes_string(x = comp_a,y = comp_b))+
    geom_point(size = size,alpha=alpha,pch=mypch,colour="#414141",stroke = stroke,aes(fill = get(col_val)))+
    #scale_fill_manual(values = mix.colors)+
    xlab(label = paste(comp_a,"(",percvar[which(names(percvar)==comp_a)],"%)",sep="")) + 
    ylab(label = paste(comp_b,"(",percvar[which(names(percvar)==comp_b)],"%)",sep="")) +
    guides(fill = guide_legend(override.aes = list(size=12))) +
    theme(axis.line = element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour =   "#D9D9D9"),
          panel.grid.minor = element_line(colour = "#D9D9D9"),
          panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
          axis.ticks = element_line(colour = "black",size = 2.5),
          axis.text.x = element_text(family = "AvantGarde",face = "plain",size =20,colour="#414141"),
          axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
          axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
          axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
          legend.background = element_blank(),legend.key.size = unit(2,"point"),
          legend.title=element_blank(),legend.key = element_blank(), 
          legend.text = element_text(size=20,
                                     family = "AvantGarde",face = "plain",colour = "#414141"),
          legend.position ="right") 
  p<-p + guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=10,stroke=stroke,shape=mypch,alpha=alpha)))
  #Add the total variance explained and the significance as a title for the plot
  #if(pval <0.001){
  #  p<-p + theme(plot.title = element_text(family = 'AvantGarde',colour="#414141",hjust=1,size = 30)) +labs(title=paste(format(vartot, digits=3), " % of total variance, p-value<0.001",sep=""))
  #  
  #}else{
  #  p<-p + theme(plot.title = element_text(family = 'AvantGarde',colour="#414141",hjust=1,size = 30)) +labs(title=paste(format(vartot, digits=3), " % of total variance, p-value=",format(pval, digits=3),sep=""))
  #  
  #}
  return(p)
}

