
pot<- function(obj, ...)
{
  UseMethod("pot")
}




# pot.default<- function(obj, newg, newd, ...)
# {
#   obj1<- Tree(newg, newd)
#   return(obj1)
# }


# 23.03.2020 pot.default is changed to this method in order for the function to be accessible
# by a class 'Network' 'BlockGraph', Note that
#' @export
pot.default<- function(obj, newg, newd, ...)
{
  obj1<- Network(newg, newd)
  return(obj1)
}



# modification made on pot.Network, pot.Tree

# pot.Network<- function(obj, obj2, k_ratio, ...)
# {
#   NextMethod()
# }







#pot
# Peaks over threshold graph and data
# obj Object of class \code{GTree} or \code{HRMtree}
# obj2 Object of class \code{RootDepSet}. If multiple subsets are passed only the first is considered.
# k_ratio The number of the upper order statistics as a percentage of the sample size
# ... additional arguments
# An object of the same class attribute as \code{obj} with two slots:
# \code{g} is the induced subgraph on the vertices in \code{obj2}
# \code{data} is the Peaks over threshold for the variables in the subset of \code{obj2}
#' @export
pot.Network<- function(obj, obj2, k_ratio, ...)
{
  g<- getGraph(obj)
  U_bar<- getNoDataNodes(obj)
  W<-     getValue(obj2)
  u<-     getRoot(obj2)
  if (is.list(W))
  {
    W<- W[[1]]
    u<- u[1]
    message("Only the first subset is used to compute the pot data")
  }
  W<- subset(obj, W, u)

  W_<- base::setdiff(W, union(U_bar, u))
  X<- getData(obj)[, base::union(u, W_)]

  E<- -log(1-copula::pobs(X))

  n<- nrow(X)
  k<- round(k_ratio*n)
  if (length(k)>1)
    stop("From pot.Network: k-ratio must be a scalar")
  if (k==1)
    stop(" 'k' = 1: there can not be variation in the sample, increase 'k_ratio' ")




  ##### IMPORTANT MODIFICATION #####
  # now it takes exactly the k last observations
  excIndex<- order(X[,u])[(n-k+1):n]
  deltaExc<- E[excIndex, W_] - E[excIndex, u]



  #######################################


  if (length(W_)==1)
  {
    deltaExc<- as.matrix(deltaExc)
    colnames(deltaExc)<- W_
  }
  g_u<- spanned_subgraph(g, u, W)

  NextMethod(newg=g_u, newd=deltaExc, ...)

}






#' @export
pot.GTree<- function(obj, obj2, k_ratio,  newg, newd, ...)
{
  obj1<- GTree(newg, newd)
  return(obj1)
}






#' @export
pot.BlockGraph<- function(newg, newd, ...)
{

  #x<- pot.Tree(obj, rdsobj, k_ratio)
  #class(x)<- append(class(x), "BlockGraph")
  newobj<- BlockGraph(newg, newd)
  return(newobj)


}









#' @export
pot.HRMnetwork<- function(obj,...)
{
  NextMethod()
}







#' @export
pot.HRMtree<- function(obj, obj2, ...)
{
  W<- getValue(obj2)
  u<- getRoot(obj2)
  if (is.list(W))
  {
    W<- W[[1]]
    u<- u[1]
    message("Only the first subset is used to compute the pot data")
  }

  g<- obj$graph
  g_u<- spanned_subgraph(g, u, W)
  obj1<- HRMtree(g_u)
  return(obj1)

}




#' @export
pot.HRMBG<-  function(obj, obj2, ...)
{
  W<- getValue(obj2)
  u<- getRoot(obj2)
  if (is.list(W))
  {
    W<- W[[1]]
    u<- u[1]
    message("Only the first subset is used to compute the pot data")
  }

  g<- obj$graph
  g_u<- spanned_subgraph(g, u, W)
  obj1<- HRMBG(g_u)
  return(obj1)

}



#
# pot.BlockGraph<- function(obj, obj2, k_ratio, ...)
# {
#   g<- getGraph(obj)
#   U_bar<- getNoDataNodes(obj)
#   W<-     getValue(obj2)
#   u<-     getRoot(obj2)
#   if (is.list(W))
#   {
#     W<- W[[1]]
#     u<- u[1]
#     message("Only the first subset is used to compute the pot data")
#   }
#   W<- subset(obj, W, u)
#
#   W_<- base::setdiff(W, union(U_bar, u))
#   X<- getData(obj)[, base::union(u, W_)]
#
#   E<- -log(1-copula::pobs(X))
#
#   n<- nrow(X)
#   k<- round(k_ratio*n)
#   if (k==1)
#     stop(" 'k' = 1: there can not be variation in the sample, increase 'k_ratio' ")
#
#
#
#
#   ##### IMPORTANT MODIFICATION #####
#   # now it takes exactly the k last observations
#   excIndex<- order(X[,u])[(n-k+1):n]
#   deltaExc<- E[excIndex, W_] - E[excIndex, u]
#
#
#
#   #######################################
#
#
#   if (length(W_)==1)
#   {
#     deltaExc<- as.matrix(deltaExc)
#     colnames(deltaExc)<- W_
#   }
#   g_u<- spanned_subgraph(g, u, W)
#
#   newobj<- BlockGraph(g_u, deltaExc)
#   return(newobj)
#
# }



