## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
library(gremes)


## -----------------------------------------------------------------------------
data("SeineData", package = "gremes")

head(Seine)


## -----------------------------------------------------------------------------
seg<- graph(c(1,2,
              2,3,
              2,4,
              4,5,
              5,6,
              5,7), directed = FALSE)
name_stat<- c("Paris", "2", "Meaux", "Melun", "5", "Nemours", "Sens")
seg<- set.vertex.attribute(seg, "name", V(seg), name_stat) # 


## -----------------------------------------------------------------------------
tobj<- Tree(seg, Seine)
Uc<- getNoDataNodes(tobj)


## -----------------------------------------------------------------------------
subs<- Neighborhood()
subs<- subset(subs, 3, seg, Uc) # neighborhood of order three

# verify if the identifiability criterion is satisfied for every subgraph induced by a subset
is_identifiable(subs, tobj) 

# change the order of the neighborhood and verify the identifiability again
subs<- subset(subs, 2, seg, Uc) # neighborhood of order two
is_identifiable(subs, tobj)


## -----------------------------------------------------------------------------
mle1<- MLE1(seg)
mle1<- estimate(mle1, Seine, subs, k_ratio=0.2)


## -----------------------------------------------------------------------------
mle2<- MLE2(seg)
mle2<- estimate(mle2, Seine, subs, k_ratio=0.2)


## -----------------------------------------------------------------------------
mle1$depParams
mle2$depParams


## -----------------------------------------------------------------------------
seg_short<- graph(c(1,2,
              2,3,
              2,4,
              2,5), directed = FALSE)
name_stat<- c("Paris", "Melun", "Meaux", "Nemours", "Sens")
seg_short<- set.vertex.attribute(seg_short, "name", V(seg_short), name_stat) # 


## -----------------------------------------------------------------------------
tobj<- Tree(seg_short, Seine)
Uc<- getNoDataNodes(tobj)


## -----------------------------------------------------------------------------
subs_short<- Neighborhood()
subs_short<- subset(subs, 2, seg_short, Uc) # neighborhood of level three


## -----------------------------------------------------------------------------
mle<- MLE(seg_short)
mle<- estimate(mle, Seine, subs_short,  k_ratio=0.2)


## -----------------------------------------------------------------------------
sqrt(mle$depParams)


