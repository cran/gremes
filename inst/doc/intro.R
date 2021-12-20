## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(gremes)


## ----fig1, fig.height = 5, fig.width = 5--------------------------------------
seg<- graph(c(1,2,
              2,3,
              2,4,
              4,5,
              5,6,
              5,7), directed = FALSE)
name_stat<- c("Paris", "2", "Meaux", "Melun", "5", "Nemours", "Sens")
seg<- set.vertex.attribute(seg, "name", V(seg), name_stat) 
plot(seg, edge.label=c("t1", "t2", "t3", "t4", "t5", "t6"))


## ---- fig.height = 5, fig.width = 5-------------------------------------------
g<- graph(c(1,3,1,2,2,3,
            3,4,4,5,5,3,
            3,7,3,6,6,7), directed=FALSE)
g<- set.vertex.attribute(g, "name", V(g), letters[1:7])
plot(g, edge.label=c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9"))


