## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
suppressMessages(library(ohchibi))

## ------------------------------------------------------------------------
data(syncom)
Dat

## ----fig.width = 8,fig.height = 6,fig.show = "hold"----------------------

#Define Bray Curtis as the dissimilarity
distfun <- function(x,method) vegan::vegdist(x = x, method = "bray")

#Run the oh.pco function
mpco <- oh.pco(Tab = Dat$Tab %>% t, Map = Dat$Map,
       distfun = distfun,id_var = "ID_Matrix")

#Plot the results using chibi.pco
p_pco <- chibi.pco(list_ohpco = mpco,
          col_val = "Fraction",size = 8,alpha = 1,size_panel_border = 2)

p_pco

## ----fig.width = 6,fig.height = 8,fig.show = "hold"----------------------

#Use vegan adonis function to compute permanova
Tab_bray <- distfun(t(Dat$Tab))
mypermanova <- adonis(Tab_bray ~  Fraction + Phosphate,
                      data = Dat$Map,strata = Dat$Map$Rep,
                      permutations = 999)

mypermanova


#Plot the results using chibi.permanova
p_perm <- chibi.permanova(mypermanova = mypermanova)
p_perm$p 



## ----fig.width = 9,fig.height = 8,fig.show = "hold"----------------------
a <- p_perm$p + theme(legend.position = "none")
b <- p_pco + theme(legend.position = "none")
egg::ggarrange(a,b,nrow = 1,widths = c(0.05,1))

## ----fig.width = 9.5,fig.height = 6,fig.show = "hold"--------------------

#Run the oh.cap function
Dat_sub <- Dat %>% subset.Dataset(Fraction == "Root",drop = T,clean = T)
mcap <- oh.cap(Tab = Dat_sub$Tab, Map = Dat_sub$Map,
                  formula = "Phosphate + Condition(Rep)",
                  distfun = distfun,perms = 9999)

#Plot the results using chibi.cap
chibi.cap(list_ohpco = mcap,comp_a = "CAP1",comp_b = "CAP2",
          col_val = "Phosphate",size = 8,alpha = 1,size_panel_border = 2)

## ----fig.width = 9.5,fig.height = 6,fig.show = "hold"--------------------


#Collapse taxonomy to the phylum level
Dat_phyla <- Dat %>% collapse_by_taxonomy.Dataset(Dat = .,level = 3)

res <- chibi.phylogram(Tab = Dat_phyla$Tab,Map = Dat_phyla$Map,
                       facet_formula = "Fraction",
                       size_ticks_x = 0,size_strip_text = 35,size_axis_text = 25,
                       legend_proportion_size = 0.5,,size_legend_text = 30,
                  size_axis_title = 0,font_family = "Arial",size_title_text = 35)


## ----fig.width = 9.5,fig.height = 6,fig.show = "hold"--------------------
res$p_raw + theme(legend.position = "none")
res$p_mean + theme(legend.position = "none")


## ----fig.width = 9.5,fig.height = 6,fig.show = "hold"--------------------
res <- chibi.phylogram(Tab = Dat_phyla$Tab,Map = Dat_phyla$Map,
                       facet_formula = "Fraction+Phosphate",
                       size_ticks_x = 0,size_strip_text = 35,size_axis_text = 25,
                       legend_proportion_size = 0.5,,size_legend_text = 30,
                  size_axis_title = 0,font_family = "Arial",size_title_text = 35)

res$p_raw + theme(legend.position = "none",
                  strip.text.x = element_text(angle = 90,size = 5,vjust = 0.5,hjust = 1))
res$p_mean + theme(legend.position = "none",
                   strip.text.x = element_text(angle = 90,size = 5,vjust = 0.5,hjust = 1))

