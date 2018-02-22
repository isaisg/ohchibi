#' Performs Constrained Analysis of Principal Coordinates based on vegan capscale
#'
#' Performs CAP using capscale and automatically computes significance of the model and terms based on permutation. As well automatically extract constrained and unconstrained variances.
#' @keywords cap
#' @export
#' @examples
#' oh.cap()


oh.cap<-function(Tab=NULL,Map=NULL,formula=formula,distfun=distfun,perms=5000,sqrt=TRUE){
  formula<-as.formula(paste("t(Tab) ~",formula))
  cap <- vegan::capscale(formula = formula,data = Map,dfun = distfun,sqrt.dist = sqrt)
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

