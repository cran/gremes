## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
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
seg<- set.vertex.attribute(seg, "name", V(seg), name_stat) 


## -----------------------------------------------------------------------------
subs<- Neighborhood()
subs<- subset(subs, 2, seg, U_bar=c("2", "5"))
subs 


## -----------------------------------------------------------------------------
mme<- MME(seg)
mme<- estimate(mme, Seine, subs,  k_ratio=0.2)


## -----------------------------------------------------------------------------
sqrt(mme$depParams)


