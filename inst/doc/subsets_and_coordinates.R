## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gremes)


## -----------------------------------------------------------------------------
rdsobj<- RootDepSet()
rdsobj<- setRootDepSet(rdsobj, subset = list(c("a", "b", "c"), c("b", "c")), root = c("a", "b"))
rdsobj


## -----------------------------------------------------------------------------
seg<- graph(c(1,2,
              2,3,
              2,4,
              4,5,
              5,6,
              5,7), directed = FALSE)
name_stat<- c("Paris", "2", "Meaux", "Melun", "5", "Nemours", "Sens")
seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)  
subs<- Neighborhood()
subset(subs, 3, seg)

# passing the latent variables
subset(subs, 2, seg, U_bar=c("2", "5"))


## ---- fig.height=4.5, fig.width=4.5-------------------------------------------
# create a graph and name the vertices
g<- graph(c(1,2,3,2, 2,4,4,5), directed=TRUE)
g<- set.vertex.attribute(g, "name", V(g), letters[1:5])

# create the flow connection matrix if not available 
x<- matrix(rep(1,25), 5, 5)
x[1,3]<- x[3,1]<- 0
colnames(x)<- rownames(x)<- letters[1:5] # note that the columns and rows should be named according to the nodes

# create the object of class 'FlowConnectionMatrix'
fcmat<- FlowConnectionMatrix(x,g)
fcmat
plot(g)


## ----fig.height=4.5, fig.width=4.5--------------------------------------------

g<- graph(c(1,2,3,2, 2,4,4,5), directed=TRUE)
g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c", "d", "e"))
fcg<- FlowConnectionGraph(g)
plot(g)
flowConnection(fcg)


## ----fig.height=7, fig.width=7------------------------------------------------
data("DanubeGraph", "DanubeFlowConnectedNodes", "DanubeData", package = "gremes")
plot(DanubeTree, layout=layout_as_tree(DanubeTree, root=c(1)))


## -----------------------------------------------------------------------------
# if the source object is a matrix, create an object "FlowConnectionMatrix"
fcmat<- FlowConnectionMatrix(DanubeFlow, DanubeTree)
rds<- FlowConnect()
sets<- subset(rds, from = fcmat, DanubeTree)
sets$value$X3
sets$value$X21


## -----------------------------------------------------------------------------
# if the source object is a directed graph, create an object "FlowConnectionGraph"
fcg<- FlowConnectionGraph(g)
sets<- subset(rds, fcg, g)
sets


## -----------------------------------------------------------------------------
tup<- Tuples()
seg<- make_tree(8,2, mode = "undirected")
seg<- set.vertex.attribute(seg, "name", V(seg), letters[1:8]) # 
data<- matrix(rnorm(10*8), 10,8)
colnames(data)<- letters[1:8]

tobj<- Tree(seg, data)
x<- rep(1,8)
names(x)<- get.vertex.attribute(tobj$graph, "name", V(tobj$graph))
ep<- evalPoints(tup, tobj, x)
head(ep)


## -----------------------------------------------------------------------------
x<- c(1:8)
names(x)<- get.vertex.attribute(tobj$graph, "name", V(tobj$graph))
ep<- evalPoints(tup, tobj, x)
head(ep)


## -----------------------------------------------------------------------------
tri<- Triples()
ep<- evalPoints(tri, tobj, x)
head(ep)

quad<- Quadruples()
ep<- evalPoints(quad, tobj, x)
head(ep)

adj<- Adjacent()
ep<- evalPoints(adj, tobj, x)
head(ep)


## ----fig.height=4.5, fig.width=4.5--------------------------------------------
seg<- graph(c(1,2,
              2,3,
              2,4,
              4,5,
              5,6,
              5,7), directed = FALSE)
name_stat<- c("Paris", "2", "Meaux", "Melun", "5", "Nemours", "Sens")
seg<- set.vertex.attribute(seg, "name", V(seg), name_stat) 
plot(seg)


## -----------------------------------------------------------------------------
# we need some data to create the object of class "Tree"
seg_data<- matrix(rnorm(10*7), 10, 7)
colnames(seg_data)<- name_stat
tobj<- Tree(seg, seg_data[,c("Paris", "Meaux", "Melun", "Nemours", "Sens")])

# create the neighborhood of order one and call the function "is_identifiable" 
nobj<- Neighborhood()
nobj<- subset(nobj, 1, seg, U_bar=getNoDataNodes(tobj))
is_identifiable(nobj, tobj)


## -----------------------------------------------------------------------------
nobj<- subset(nobj, 2, seg, U_bar=getNoDataNodes(tobj))
is_identifiable(nobj, tobj)


