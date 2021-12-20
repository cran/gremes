## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(gremes)


## ----fig.height=4.5, fig.width=4.5--------------------------------------------
g<- graph(c(1,3,1,2,2,3,
            3,4,4,5,5,3,
            3,7,3,6,6,7), directed=FALSE)
g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c", "d", "e", "f", "g"))
plot(g)


## -----------------------------------------------------------------------------
# all deltas are squares already
C1<- c(0.2, 0.8, 0.6)     # d_13^2, d_12^2, d_23^2
C2<- c(0.3, 0.5, 0.1)     # d_34^2, d_45^2, d_35^2
C3<- c(0.4, 0.05, 0.25)   # d_37^2, d_36^2, d_67^2


## -----------------------------------------------------------------------------
hrmbgobj<- HRMBG(g)
hrmbgobj<- setParams(hrmbgobj, c(C1, C2, C3))
hrmbgobj


## -----------------------------------------------------------------------------
hrmlam<- HRLambda(hrmbgobj)
hrmlam


## -----------------------------------------------------------------------------
X<- rHRM(hrmbgobj, hrmlam, 1000, noise = TRUE)


## -----------------------------------------------------------------------------
rdsobj<- RootDepSet()
rdsobj<- setRootDepSet(rdsobj, subset = list(c("a", "b", "d", "e", "f", "g"),
                                             c("a", "b", "d", "e", "f", "g"),
                                             c("a", "b", "d", "e", "f", "g"),
                                             c("a", "b", "d", "e", "f", "g"),
                                             c("a", "b", "d", "e", "f", "g"),
                                             c("a", "b", "d", "e", "f", "g")), 
                       c("a", "b", "d", "e", "f", "g"))


## -----------------------------------------------------------------------------
hrmbg<- HRMBG(g)
hrmbg<- suppressMessages(estimate(hrmbg, X[,-3], rdsobj, k_ratio=0.2))
hrmbg$depParams


