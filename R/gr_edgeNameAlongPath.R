# Obtains the edge names along a path between two vertices on a tree
#' @import igraph
# edge_names_along_path
edge_names_along_path<- function(obj, ...)
{
  UseMethod("edge_names_along_path", obj)
}




#' @export
edge_names_along_path.default<- function(obj, ...)
{
  return("Default method called on unrecognized function")
}




# edge_names_along_path
# obj object of class \code{HRMnetwork}
# rt the name of the node from which the path is requested
# id The name of the node to which the path is requested
# edge_names Returns a vector with the names of the edges. Default is TRUE. If FALSE, it returns
# a named vector of ones and zeros with ones if the corresponding edge is part of the path
# ... additional arguments
# A vector with the names of the edges along the path between \code{rt} and \code{id}
#' @export
edge_names_along_path.HRMnetwork<- function(obj, rt, id, edge_names=TRUE, ...)
{

  #debug
  #rt<- "Melun"
  #id<- "5"
  #edge_names=TRUE

  #--------


  # bool is a boolean variable:
  # if bool = TRUE the function returns a vector with the indices of the edges from rt to id
  # if bool = FALSE the function returns a vector of length the number of edges,
  # with ones and zeros. The vector has one if the corresponding theta is involved in the path
  # between rt and id and zero otherwise.
  #obj #$treeGraph

  g<- obj$graph
  # vsp<- igraph::get.all.shortest.paths(g, rt,id)$res[[1]]
  # xx<- get.vertex.attribute(g, "name", vsp)
  # e<- character(length(xx)-1)
  # for (i in 2:length(xx))
  # {
  #   e[i-1]<- get.edge.attribute(g, "name", get.edge.ids(g, c(xx[i],xx[i-1])))
  # }

  sp<- unlist(get.shortest.paths(g, rt, id)$vpath)
  sp1<- rep(sp, rep(2, length(sp)))
  sp1<- sp1[2:(length(sp1)-1)]
  geids<- get.edge.ids(g, sp1)
  enames<- get.edge.attribute(g, "name", geids)

  if (edge_names)
  {
    return(enames)
  } else {
    edge_path<- rep(0, igraph::ecount(g))
    names(edge_path)<- get.edge.attribute(g, "name", index = E(g))
    edge_path[enames]<- 1
    return(edge_path)
  }
}

