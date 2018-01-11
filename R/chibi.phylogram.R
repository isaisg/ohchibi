#' Plots a proportion barplot(phylogram) 
#'
#' 
#' @keywords phylogram
#' @export
#' @examples
#' chibi.phylogram()


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


