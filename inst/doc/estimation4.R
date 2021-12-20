## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
# devtools::load_all("~/gremes", export_all=FALSE)
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
seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)


## -----------------------------------------------------------------------------
subs<- list(c("Paris", "2", "Meaux", "Melun"), c("Melun", "5", "Nemours", "Sens"))


## -----------------------------------------------------------------------------
rdsobj<- RootDepSet()
rdsobj<- setRootDepSet(rdsobj, subs, c("Paris", "Melun"))
tobj<- Tree(seg, Seine)
is_identifiable(rdsobj, tobj)


## -----------------------------------------------------------------------------
ehobj<- EngHitz(seg)
ehobj<- suppressMessages(estimate(ehobj, Seine, subs, k_ratio=0.2))
ehobj$depParams


