## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(gremes)


## -----------------------------------------------------------------------------
#load("~/gremes/data/SeineData.RData")
data("SeineData", package = "gremes")

head(Seine)


## ----fig.height=4.5, fig.width=4.5--------------------------------------------
seg<- graph(c(1,2,
              2,3,
              2,4,
              4,5,
              5,6,
              5,7), directed = FALSE)
name_stat<- c("Paris", "2", "Meaux", "Melun", "5", "Nemours", "Sens")
seg<- set.vertex.attribute(seg, "name", V(seg), name_stat) # 
plot(seg)


## -----------------------------------------------------------------------------
tobj<- Tree(seg, Seine) 
Uc<- getNoDataNodes(tobj)


## -----------------------------------------------------------------------------
mme_ave<- MMEave(seg)
estimate(mme_ave, Seine, k_ratio=0.2)$depParams


## -----------------------------------------------------------------------------
mle_ave<- MLEave(seg)
estimate(mle_ave, Seine, k_ratio=0.2)$depParams


