## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
library(gremes)


## -----------------------------------------------------------------------------
#load("~/gremes/data/SeineData.RData")
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
Uc<- c("2", "5")


## -----------------------------------------------------------------------------
tup<- Tuples()
x<- rep(1,5)
names(x)<- getNodesWithData(tobj)
tup<- evalPoints(tup, tobj, x)


## -----------------------------------------------------------------------------
eks<- EKS(seg)
eks<-estimate(eks, Seine, tup, k_ratio=0.2)


## -----------------------------------------------------------------------------
subs<- Neighborhood()
subs<- subset(subs, 2, seg, Uc) # neighborhood of level two


## -----------------------------------------------------------------------------
eks_part<- EKS_part(seg)
eks_part<- suppressMessages(estimate(eks_part, Seine, subs, k_ratio=0.2, xx=x))


## -----------------------------------------------------------------------------
eks$depParams
eks_part$depParams


