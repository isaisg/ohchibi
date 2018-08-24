#' Plots a proportion barplot(phylogram) 
#'
#' 
#' @keywords phylogram
#' @export
#' @examples
#' chibi.phylogram()


chibi.phylogram<-function (Tab = NULL, Map = NULL, facet_formula = NULL, colname = "Sample", 
                        variable.name = "Taxon", value.name = "Abundance", scales = "free_x", 
                        space = "free_x",width_bars = 1,spacing_x = 0.4,legend_proportion_size =2, nrow.legend = 20, ntaxa = NULL,
                        other_name = "Other",funsum="mean",y_vjust=0.5,size_axis_text=20,
                        size_axis_title=30,size_legend_text=20,size_strip_text=10,size_ticks_x = 2.5,size_ticks_y =2.5) {
  #Die if not Tab and Map was passed
  if(is.null(Tab) ){
      stop("ERROR: You should at least pass a Matrix (Tab)",call.=TRUE)
  }
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
  if (!is.null(Map) & !is.null(facet_formula)) {
    Map <- Map[row.names(Tab), ]
    colnamep <- c(colname, names(Map))
    Tab <- cbind(Tab, Map)
    Dat <- melt(Tab, id.vars = colnamep, variable.name = variable.name, 
              value.name = value.name)
  }else{
    Dat <- melt(Tab, id.vars = colname, variable.name = variable.name, 
              value.name = value.name)
  }
  p1 <- ggplot(Dat, aes_string(x = colname, y = value.name, 
      fill = variable.name)) + geom_bar(stat = "identity", 
    position = "fill", width = width_bars) + coord_cartesian( ylim=c(0,1), expand = FALSE ) +
    theme(
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.spacing.x = unit(spacing_x, "lines"),
          axis.ticks.y =element_line(colour = "black",size = size_ticks_y),
          axis.ticks.x =element_line(colour = "black",size = size_ticks_x),
          axis.text.x =element_blank(),
          axis.text.y = element_text(family = "AvantGarde",face="plain",size=size_axis_text,colour="#414141",vjust = y_vjust),
          axis.title.x = element_text(family = "AvantGarde",face="plain",size = size_axis_title,colour = "#414141"),
          axis.title.y = element_text(family = "AvantGarde",face="plain",size=size_axis_title,colour="#414141"),
          legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
          legend.title=element_blank(),legend.key = element_blank(),
          legend.text = element_text(size=size_legend_text,family = "AvantGarde",face = "plain",colour = "#414141"),
          legend.position ="right",strip.text.x = element_text(family = "AvantGarde",colour = "#414141",size = size_strip_text),
          strip.background = element_blank(),
          )
  if (!is.null(Map) & !is.null(facet_formula)) {
    mformula <- gsub(pattern = " ",replacement = "",x = facet_formula)
    facet_formula <- as.formula(paste("~", facet_formula))
    p1 <- p1 + facet_grid(facet_formula, scales = scales, space = space)
  }
  p1 <- p1 + guides(fill = guide_legend(nrow = nrow.legend))
  #Here evaluate if the mformula was passed at all
  if( is.null(facet_formula) ){
    toret=list(p_raw = p1)
    return(toret)
  }else{
  #Take the Dat object and aggregate based on the given facette
  ##Do the mean
  #Remove white spaces from the formula if provided
  formula <- as.formula(paste0("Abundance ~ Taxon +",mformula))
  temp <- aggregate(formula,data=Dat,funsum)
  cols_pick <- unlist(strsplit(x = mformula,split = "\\+"))
  temp2 <- temp[,match(cols_pick,colnames(temp))]
  if(is.null(dim(temp2))){
    temp$Sample<-temp2
  }else{
    temp$Sample<-factor(apply(temp2,1,paste,collapse="_"))
  }
  p2 <- ggplot(temp, aes_string(x = "Sample", y = "Abundance", 
    fill = "Taxon")) + geom_bar(stat = "identity", 
      position = "fill", width = width_bars) + coord_cartesian( ylim=c(0,1), expand = FALSE ) +
    theme(
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.spacing.x = unit(spacing_x, "lines"),
          axis.ticks.y =element_line(colour = "black",size = size_ticks_y),
          axis.ticks.x =element_line(colour = "black",size = size_ticks_x),
          axis.text.x =element_blank(),
          axis.text.y = element_text(family = "AvantGarde",face="plain",size=size_axis_text,colour="#414141",vjust = y_vjust),
          axis.title.x = element_text(family = "AvantGarde",face="plain",size = size_axis_title,colour = "#414141"),
          axis.title.y = element_text(family = "AvantGarde",face="plain",size=size_axis_title,colour="#414141"),
          legend.background = element_blank(),legend.key.size = unit(legend_proportion_size,"line"),
          legend.title=element_blank(),legend.key = element_blank(),
          legend.text = element_text(size=size_legend_text,family = "AvantGarde",face = "plain",colour = "#414141"),
          legend.position ="right",strip.text.x = element_text(family = "AvantGarde",colour = "#414141",size = size_strip_text),
          strip.background = element_blank(),
          )
  if (!is.null(Map) & !is.null(facet_formula)) {
    p2 <- p2 + facet_grid(facet_formula, scales = scales, space = space)
  }
  toret=list(p_raw = p1, p_mean=p2)
  return(toret)
  }
 
}


