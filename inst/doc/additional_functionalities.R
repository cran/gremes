## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gremes)


## ----fig.height=4.5, fig.width=4.5--------------------------------------------
seg<- make_tree(8,3, mode = "undirected") # create the tree
seg<- set.vertex.attribute(seg, "name", V(seg), letters[1:8]) # name the nodes
hrm<- HRMtree(seg) # initialize the object of of class HRMtree 
hrm<- setParams(hrm, seq(0.1, 0.7, 0.1)) # set its parameters
X<- rHRM(hrm, 1000) # generate a random sample 
round(head(X), 4)
XX<- rHRM(hrm, 1000, noise = TRUE) # generate a random samle with independent normal noise
round(head(XX), 4)
plot(seg)


## ----fig.height = 4.5, fig.width = 4.5----------------------------------------
diagnost(hrm, X, "b", y = c(0.3,0.5))


## ----fig.height = 4.5, fig.width = 4.5----------------------------------------
diagnost(hrm, XX, "b", y = c(0.3, 0.5))


## ----fig.height = 4.5, fig.width = 4.5----------------------------------------
diagnost(hrm, XX, "b", y = c(0.8, 0.9))


## ----fig.height=4.5, fig.width=4.5--------------------------------------------
bg<- graph(c(1,2,2,3,1,3,
             3,4,3,5,4,5,
             3,7,3,6,6,7), directed = FALSE) # create the graph
bg<- set.vertex.attribute(bg, "name", V(bg), letters[1:7]) # name the nodes
hrbg<- HRMBG(bg) # initialize an object with zero dependence parameters
hrbg<- setParams(hrbg, seq(0.1, 0.9, 0.1)) # set the parameters
lam<- HRLambda(hrbg) # compute the structured matrix Lambda, see Vignette "Huesler-Reiss distributions"
XB<- rHRM(hrbg, lam, 1000, noise = TRUE) 
round(head(XB), 4)
plot(bg)


## -----------------------------------------------------------------------------
# non-parametric estimates on an object containing a tree 
x<- runif(8)
names(x)<- letters[1:8]
tobj<- Tree(seg, XX) # an object of containing block graph and the data associated to it
stdf(tobj, x, 0.2)
x<- x[3:8] # with latent variables on nodes "a" and "b"
names(x)<- letters[3:8] 
XU<- X[,3:8] 
tobjU<- Tree(seg, XU) # an object containing a tree an the data associated to it
stdf(tobjU, x, 0.25) 
x<- matrix(runif(40), 5,8)
colnames(x)<- letters[1:8]
stdf(tobj, x, 0.25)

# non-parametric estimates on an object containing a block graph
x<- runif(7)
names(x)<- letters[1:7]
bgobj<- BlockGraph(bg, XB) # an object containing a tree an the data associated to it
stdf(bgobj, x, 0.15)

# parametric estimates on model with respect to a tree
x<- c(0, 0.1, 0, 2.5, 0, 1.3, 2.3, 1.5)
names(x)<- letters[1:8]
stdf(hrm, x )

# parametric estimates on a model with respect to a block graph
x<- runif(7)
names(x)<- letters[1:7]
stdf(hrbg, x)


## -----------------------------------------------------------------------------
extrCoeff(bgobj, 0.2) 
extrCoeff(tobj, 0.2)  


## -----------------------------------------------------------------------------
y<- c(1, 0, 1, 1, 1, 0, 0) 
names(y)<- letters[1:7]
extrCoeff(bgobj, 0.25, y) 
extrCoeff(tobj, 0.25, y)


## -----------------------------------------------------------------------------
# on the tree 
extrCoeff(tobjU, 0.2)

# on the block graph
XBU<- XB[, -3] 
bgobjU<- BlockGraph(bg, XBU)
extrCoeff(bgobjU, 0.3)


## -----------------------------------------------------------------------------
v<- c(0,0,1,1,0,1,0,1)
names(v)<- letters[1:8]
extrCoeff(tobjU, 0.2, v)

v<- c(1, 1, 0, 1, 0, 0, 1)
names(v)<- letters[1:7]
extrCoeff(bgobjU, 0.15, v)


## -----------------------------------------------------------------------------
extrCoeff(hrm) # bivariate
v<- c(0,0,1,1,0,1,0,1)
names(v)<- letters[1:8]
extrCoeff(hrm, v) # for a particular set of variables 


## -----------------------------------------------------------------------------
extrCoeff(hrbg) # bivariate
v<- c(0,0,1,1,0,1,0)
names(v)<- letters[1:7]
extrCoeff(hrbg, v) # for a particular set of variables 


## -----------------------------------------------------------------------------
v<- c(1,0,1,1,1,0,0,0)
names(v)<- letters[1:8]
suppressMessages(taildepCoeff(hrm, v)) # parametric tdc
taildepCoeff(tobj, 0.2, v) # non-parametric tdc


## -----------------------------------------------------------------------------
# create the matrix of evaluation points 
tup<- Tuples()
x<- rep(1, 8)
names(x)<- letters[1:8]
pair<- evalPoints(tup, tobj, x)
# create an object of class EKS with the supposed estimates of the parameters
eks<- EKS(seg)
eks<- setParams(eks, seq(0.1, 0.8, 0.1))
suppressMessages(confInt(eks, pair, 150))


## -----------------------------------------------------------------------------
x<- rep(1, 6)
names(x)<- letters[3:8]
pairU<- evalPoints(tup, tobjU, x )
suppressMessages(confInt(eks, pairU, 150))


