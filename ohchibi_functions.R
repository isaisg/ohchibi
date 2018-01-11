library(AMOR)
library(ggplot2)
library(labdsv)
library(vegan)


#Function to perform pco and plot the pco object
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

chibi.pco<-function(list_ohpco=NULL,col_val=NULL,shape_val=NULL,comp_a="PCo1",comp_b="PCo2",mypch=22,size=25,alpha=0.7,stroke=1.5){
  Map_pco<-list_ohpco$Map_pco
  myvar=list_ohpco$variance_explained
  if(is.null(shape_val)){
    p <- ggplot(data = Map_pco,aes_string(x = comp_a,y = comp_b))+
      geom_point(size = size,alpha=alpha,pch=mypch,colour="#414141",stroke = stroke,aes(fill = get(col_val)))+
      #scale_fill_manual(values = mix.colors)+
      xlab(label = paste(comp_a,"(",myvar[which(names(myvar)==comp_a)],"%)",sep="")) + 
      ylab(label = paste(comp_b,"(",myvar[which(names(myvar)==comp_b)],"%)",sep="")) +
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
    return(p)
  }else{
    p <- ggplot(data = Map_pco,aes_string(x = comp_a,y = comp_b))+
      geom_point(size = size,alpha=alpha,stroke = stroke,aes_string(colour = col_val,shape = shape_val))+
      xlab(label = paste(comp_a,"(",myvar[which(names(myvar)==comp_a)],"%)",sep="")) + 
      ylab(label = paste(comp_b,"(",myvar[which(names(myvar)==comp_b)],"%)",sep="")) +
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
    return(p)
  }
}


#Function to save a ggplot object
oh.ggsave.svg<-function(ggobject=p,outname="",outdir="figures/",width=20,height=15,device="svg",dpi=600){
  dir.create(outdir, showWarnings = FALSE)
  myfilename<-paste(outdir,outname,sep="")
  ggsave(filename = myfilename,plot = ggobject,width =width,height = height,device = device,dpi = dpi)
}

oh.ggsave.jpeg<-function(ggobject=p,outname="",outdir="figures/",width=20,height=15,device="jpeg"){
  dir.create(outdir, showWarnings = FALSE)
  myfilename<-paste(outdir,outname,sep="")
  ggsave(file = myfilename,plot = ggobject,width =width,height = height,device = device)
}



##Function to make boxplots using ggplot2
chibi.boxplot<-function(Map=Map,x_val=NULL,y_val=NULL,col_val=NULL,shape_val=NULL,mypch=21,size=10,alpha=0.7,stroke=0.5){
  p <- ggplot(Map, aes_string(x = x_val, y = y_val, col = col_val,  fill = col_val)) + 
    geom_boxplot(fill = NA, outlier.colour = NA, position = position_dodge(width = 0.9), size=2) +  
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
                                                      jitter.width = 0.1), size = size, shape = mypch, col = "#414141",stroke=stroke,alpha=alpha)
    #Define the guides. They gotta be constant
    p <- p +guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=3,stroke=stroke,shape=mypch,alpha=alpha)))
    
  }else{
    p<-p + geom_point(position = position_jitterdodge(dodge.width = 0.9, 
                                                      jitter.width = 0.1), 
                      size = size,stroke=stroke,alpha=alpha,aes_string(x=x_val,y=y_val,col=col_val,fill=col_val,shape=shape_val),inherit.aes = F)
    p <- p +guides(fill=guide_legend(keywidth=0.5,keyheight=0.5,default.unit="inch",override.aes = list(size=3,stroke=stroke,alpha=alpha)))
    
  }
  
  return(p)
}

##Function to perform cca based on vegan
oh.cap<-function(Tab=NULL,Map=NULL,formula=formula,distfun=distfun,perms=5000){
  formula<-as.formula(paste("t(Tab) ~",formula))
  cap <- vegan::capscale(formula = formula,data = Map,dfun = distfun,sqrt.dist = T)
  #Compute summary of the cap
  cap_sum <- summary(cap)
  perm_anova_model <- anova.cca(object = cap,permutations = perms)
  perm_anova_terms<-anova.cca(object = cap,permutations = perms,by = "terms")
  #perm_anova_axis<-anova.cca(object = cap,permutations = perms,by = "axis")
  pval_model <- perm_anova_model[1,4]
  #Extract the variance
  Map <- cbind(Map,cap_sum$sites)
  percvar <- round(100 * cap$CCA$eig / cap$CCA$tot.chi,2)
  #Extract the porportion explained by the MDS
  eig <- cap$CA$eig
  mds_var<-round(100 * eig/sum(eig),2)
  percvar<-c(percvar,mds_var)
  #Extract the variance
  chi <- c(cap$tot.chi,cap$CCA$tot.chi, cap$CA$tot.chi)
  variability_table <- cbind(chi, chi/chi[1])
  colnames(variability_table) <- c("inertia", "proportion")
  rownames(variability_table) <- c("total", "constrained", "unconstrained")
  vartot<-variability_table[2,2]*100
  toret=list(Map_cap = Map, cap=cap,cap_sum=cap_sum,perm_anova_model=perm_anova_model,perm_anova_terms=perm_anova_terms,
             pval_model=pval_model,variance_explained_axis=percvar,variability_table=variability_table,
             total_var=vartot)
  return(toret)
}

#Function to plot a cap analysis
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
  if(pval <0.001){
    p<-p + theme(plot.title = element_text(family = 'AvantGarde',colour="#414141",hjust=1,size = 30)) +labs(title=paste(format(vartot, digits=3), " % of total variance, p-value<0.001",sep=""))
    
  }else{
    p<-p + theme(plot.title = element_text(family = 'AvantGarde',colour="#414141",hjust=1,size = 30)) +labs(title=paste(format(vartot, digits=3), " % of total variance, p-value=",format(pval, digits=3),sep=""))
    
  }
  return(p)
}

##Phylogram function adapted from AMOR
chibi.phylogram<-function (Tab, Map = NULL, facet = NULL, colname = "Sample", 
                        variable.name = "Taxon", value.name = "Abundance", scales = "free_x", 
                        space = "free_x", nrow.legend = 20, ntaxa = NULL, other_name = "other",mformula=NULL,funsum="mean") {
  #Taken from AMOR Phylogram structure
  if (is.numeric(ntaxa)) {
    if (nrow(Tab) > ntaxa) {
      select <- rowSums(Tab)
      select <- sort(select, decreasing = TRUE)
      select <- names(select)[1:ntaxa]
      groups <- row.names(Tab)
      groups[!(groups %in% select)] <- other_name
      Tab <- collapse_matrix(x = Tab, groups = groups, 
                             dim = 1, FUN = sum)
    }
  }
  Tab <- as.data.frame(t(Tab))
  measure.vars <- names(Tab)
  Tab[, colname] <- row.names(Tab)
  if (!is.null(Map) & !is.null(facet)) {
    Map <- Map[row.names(Tab), ]
    colname <- c(colname, names(Map))
    Tab <- cbind(Tab, Map)
  }
  Dat <- melt(Tab, id.vars = colname, variable.name = variable.name, 
              value.name = value.name)
  p1 <- ggplot(Dat, aes_string(x = colname, y = value.name, 
                               fill = variable.name)) + geom_bar(stat = "identity", 
                                                                 position = "fill", width = 1) + 
    theme(axis.line = element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour =   "#D9D9D9"),
          panel.grid.minor = element_line(colour = "#D9D9D9"),
          panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
          axis.ticks = element_line(colour = "black",size = 2.5),
          axis.text.x = element_blank(),
          axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
          axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
          axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
          legend.background = element_blank(),legend.key.size = unit(2,"line"),
          legend.title=element_blank(),legend.key = element_blank(), 
          legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
          legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
          strip.background = element_rect(fill = "#D9D9D9",color = "#414141"),
          strip.text.x = element_text(family = "AvantGarde",size = 7)) 
  if (!is.null(Map) & !is.null(facet)) {
    p1 <- p1 + facet_grid(facet, scales = scales, space = space)
  }
  p1 <- p1 + guides(fill = guide_legend(nrow = nrow.legend))
  #Take the Dat object and aggregate based on the given facette
  ##Do the mean
  #Remove white spaces from the formula if provided
  mformula<-gsub(pattern = " ",replacement = "",x = mformula)
  formula<-as.formula(paste0("Abundance ~ Taxon +",mformula))
  temp<-aggregate(formula,data=Dat,funsum)
  cols_pick<-unlist(strsplit(x = mformula,split = "\\+"))
  temp2<-temp[,match(cols_pick,colnames(temp))]
  if(is.null(dim(temp2))){
    temp$Sample<-temp2
  }else{
    temp$Sample<-factor(apply(temp2,1,paste,collapse="_"))
  }
  p2 <- ggplot(temp, aes_string(x = "Sample", y = "Abundance", 
                                fill = "Taxon")) + geom_bar(stat = "identity", 
                                                            position = "fill", width = 1) + 
    theme(axis.line = element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour =   "#D9D9D9"),
          panel.grid.minor = element_line(colour = "#D9D9D9"),
          panel.border = element_rect(fill=NA,color =  "#414141",size = 1),
          axis.ticks = element_line(colour = "black",size = 2.5),
          axis.text.x = element_blank(),
          axis.text.y = element_text(family = "AvantGarde",face="plain",size=20,colour="#414141"),
          axis.title.x = element_text(family = "AvantGarde",face="plain",size = 30,colour = "#414141"),
          axis.title.y = element_text(family = "AvantGarde",face="plain",size=30,colour="#414141"),
          legend.background = element_blank(),legend.key.size = unit(2,"line"),
          legend.title=element_blank(),legend.key = element_blank(), 
          legend.text = element_text(size=25,family = "AvantGarde",face = "plain",colour = "#414141"),
          legend.position ="right",strip.text = element_text(family = "AvantGarde",colour = "#414141",size = 20),
          strip.background = element_rect(fill = "#D9D9D9",color = "#414141"),
          strip.text.x = element_text(family = "AvantGarde",size = 7)) 
  if (!is.null(Map) & !is.null(facet)) {
    p2 <- p2 + facet_grid(facet, scales = scales, space = space)
  }
  toret=list(p_raw = p1, p_mean=p2)
  return(toret)
}


