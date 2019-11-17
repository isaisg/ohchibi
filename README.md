## ohchibi

[![DOI](https://zenodo.org/badge/117052809.svg)](https://zenodo.org/badge/latestdoi/117052809)



### Introduction
ohchibi is a library with functions I recurrently utilize to analyze community data, phylogenomic and pangenome matrices. There are two types of function in the ohchibi package.<br/><br/>
The functions **oh** perform a myriad of analyses and create structued suited for plotting. <br/><br/>
The functions **chibi**   take the structure output by its corresponding **oh** function and create a plot to visualize the results. <br/><br/>

### Installation
You can install the ohchibi package using the devtools library<br/><br/>
**devtools::install_github("isaisg/ohchibi",build_vignettes = TRUE))**

### Example dataset
We will utilize the syncom dataset to show the usage of the main functions in this package. 
```{r}
data(syncom)
Dat
```

####Principal Coordinate Analysis (PCoA)
```{r,fig.width = 8,fig.height = 6,fig.show = "hold"}

#Define Bray Curtis as the dissimilarity
distfun <- function(x,method) vegan::vegdist(x = x, method = "bray")

#Run the oh.pco function
mpco <- oh.pco(Tab = Dat$Tab %>% t, Map = Dat$Map,
       distfun = distfun,id_var = "ID_Matrix")

#Plot the results using chibi.pco
p_pco <- chibi.pco(list_ohpco = mpco,
          col_val = "Fraction",size = 8,alpha = 1,size_panel_border = 2)

p_pco
```

####Permutational multivariate analysis of variance (PERMANOVA)
```{r,fig.width = 6,fig.height = 8,fig.show = "hold"}

#Use vegan adonis function to compute permanova
Tab_bray <- distfun(t(Dat$Tab))
mypermanova <- adonis(Tab_bray ~  Fraction + Phosphate,
                      data = Dat$Map,strata = Dat$Map$Rep,
                      permutations = 999)

mypermanova


#Plot the results using chibi.permanova
p_perm <- chibi.permanova(mypermanova = mypermanova)
p_perm$p 


```

####Combining figures
We can usee the **ggarrange** function in the egg package to combine multiple panels

```{r,fig.width = 9,fig.height = 8,fig.show = "hold"}
a <- p_perm$p + theme(legend.position = "none")
b <- p_pco + theme(legend.position = "none")
egg::ggarrange(a,b,nrow = 1,widths = c(0.05,1))
```

####Constrained analysis of principal coordinates (CAP)
```{r,fig.width = 9.5,fig.height = 6,fig.show = "hold"}

#Run the oh.cap function
Dat_sub <- Dat %>% subset.Dataset(Fraction == "Root",drop = T,clean = T)
mcap <- oh.cap(Tab = Dat_sub$Tab, Map = Dat_sub$Map,
                  formula = "Phosphate + Condition(Rep)",
                  distfun = distfun,perms = 9999)

#Plot the results using chibi.cap
chibi.cap(list_ohpco = mcap,comp_a = "CAP1",comp_b = "CAP2",
          col_val = "Phosphate",size = 8,alpha = 1,size_panel_border = 2)
```


####Bargraphs of taxonomy (Phylogram)
```{r,fig.width = 9.5,fig.height = 6,fig.show = "hold"}


#Collapse taxonomy to the phylum level
Dat_phyla <- Dat %>% collapse_by_taxonomy.Dataset(Dat = .,level = 3)

res <- chibi.phylogram(Tab = Dat_phyla$Tab,Map = Dat_phyla$Map,
                       facet_formula = "Fraction",
                       size_ticks_x = 0,size_strip_text = 35,size_axis_text = 25,
                       legend_proportion_size = 0.5,,size_legend_text = 30,
                  size_axis_title = 0,font_family = "Arial",size_title_text = 35)

```

The **chibi.phylogram** function creates two graphs, one with the raw values (raw) and one with average measurements (mean). The way the average is computed is dependent on the facet_formula argument.

```{r,fig.width = 9.5,fig.height = 6,fig.show = "hold"}
res$p_raw + theme(legend.position = "none")
res$p_mean + theme(legend.position = "none")

```
We can change the structure of the figure by adding terms to the facet_formula

```{r,fig.width = 9.5,fig.height = 6,fig.show = "hold"}
res <- chibi.phylogram(Tab = Dat_phyla$Tab,Map = Dat_phyla$Map,
                       facet_formula = "Fraction+Phosphate",
                       size_ticks_x = 0,size_strip_text = 35,size_axis_text = 25,
                       legend_proportion_size = 0.5,,size_legend_text = 30,
                  size_axis_title = 0,font_family = "Arial",size_title_text = 35)

res$p_raw + theme(legend.position = "none",
                  strip.text.x = element_text(angle = 90,size = 5,vjust = 0.5,hjust = 1))
res$p_mean + theme(legend.position = "none",
                   strip.text.x = element_text(angle = 90,size = 5,vjust = 0.5,hjust = 1))
```

