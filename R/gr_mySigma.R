#' Covariance matrix
#'
#' It computes either a non-parametric estimate of the covariance matrix or a parametric form of the
#' covariance matrix.
#' @param obj Object of class \code{GTree} or \code{HRMtree}
#' @param ... additional arguments
#' @return A matrix of non-parametric estimates or a matrix of zeros and ones, say A, such that A*theta is
#' the halfvectorized Sigma,
#'  where Sigma is the parametric covariance matrix.
#'  If the first argument is of class \code{GTree} the method returns the empirical covariance matrix
#'  of the data in the GTree object.
#' If the first argument is of class \code{HRMtree} the method returns
#' a matrix of ones and zeros which corresponds to \eqn{hvec(\Sigma(\theta))=A\theta}
#' @note It is not intendend for independent use.
#' @export
sigma<- function(obj, ...)
{
   UseMethod("sigma", obj)
}

#' @export
sigma.default<- function(obj,...)
{
  return("Default method called on unrecognized object")
}


# #' @export
# sigma.Network<- function(obj,...)
# {
#   NextMethod()
# }


#' @export
#' @importFrom stats cov
sigma.Network<- function(obj, ...)
{
  # print(match.call())
  sx<- getData(obj)
  if (is.null(dim(sx)))
  {
    S_hat<- var(sx)
  } else {
    S_hat<- stats::cov(sx)
  }

  NextMethod(obj, s=S_hat,...)
}








#' @export
# S_hat Empirical covariance matrix
sigma.GTree<- function(obj, s, ...)
{
  return(s)
}


#' @export
sigma.BlockGraph<- sigma.GTree




#' @export
sigma.HRMnetwork<- function(obj,...)
{
  NextMethod()
}





# obj2 Object of class \code{RootDepSet}
# U_bar Vector of the missing vertices in a graph, the default is NULL
#' @export
sigma.HRMtree<- function(obj, obj2, U_bar=NULL,...)
{
  #debug

 # obj2<- set
#  U_bar<- Uc
  #-------------


  g<- obj$graph
  W<- getValue(obj2)
  u<- getRoot(obj2)
  if(is.list(W))
  {
    W<- W[[1]]
    u<- u[1]
    message("Multiple input sets. Only the first is considered")
  }

  W_<- base::setdiff(W, base::union(u, U_bar))
  nW_<- length(W_)
  q<- c(1:nW_)
  colIndex<- unlist(sapply(q, function(x) base::setdiff(W_, W_[1:x])))
  pairsOfSigma<- base::rbind(rep(W_, c(nW_:1)), c(W_, colIndex))
  A_u<- matrix(0, nrow = ncol(pairsOfSigma), ncol = ecount(g))
  colnames(A_u)<- get.edge.attribute(g, "name", E(g))
  for (i in 1:ncol(pairsOfSigma))
  {
    edges_i<- edge_names_along_path(obj, u, pairsOfSigma[1,i])
    edges_j<- edge_names_along_path(obj, u, pairsOfSigma[2,i])
    A_u[i, base::intersect(edges_i,edges_j)]<- 1
  }
  return(A_u)


}




#' @export
sigma.MMEave<- function(obj, obj2, A_u, U_bar, ...)
{
  #debug
 # obj<- aveobj
 # A_u<- Lamb
#  nW_<- 4
  #####################



  W<- getValue(obj2)
  if(is.list(W))
  {
    W<- W[[1]]
    message("Multiple input sets. Only the first is considered")
  }

  W_<- base::setdiff(W, U_bar)
  nW_<- length(W_)


  Sigma2<- matrix(0, nrow = nrow(A_u), ncol = ncol(A_u))
  colnames(Sigma2)<- colnames(A_u)

  Sigma3<- matrix(0, nrow = nrow(A_u), ncol = ncol(A_u))
  colnames(Sigma3)<- colnames(A_u)
  j=1
  for (i in seq(0, nW_^2-nW_, nW_))
  {
    vv<- apply(A_u[(i+1):(i+nW_),], 2, sum)
    Sigma2[(i+1):(i+nW_),]<- matrix(rep(vv, nW_), nrow=nW_, ncol=ncol(Sigma2), byrow=TRUE)
    Sigma3[seq(0, nW_^2-nW_, nW_)+j,]<- matrix(rep(vv, nW_),
                                               nrow=nW_, ncol=ncol(Sigma2), byrow=TRUE)
   j<- j+1
  }
  Sigma2d<- -1/(4*nW_)*Sigma2
  Sigma3d<- -1/(4*nW_)*Sigma3

  Sigma4d<- 1/(nW_^2*4)*matrix(rep(apply(A_u, 2, sum), nrow(A_u)),
                  nrow=nrow(A_u), ncol=ncol(A_u), byrow=TRUE)

  sigmaMGM<- -(A_u/4+ Sigma2d + Sigma3d + Sigma4d)

  return(sigmaMGM)

}



#' @export
sigma.MLEave<- sigma.MMEave





# # old version
# #' @export
# sigma.HRMBG<- function(obj, rdsobj, ... )
# {
#   # the code does not take into account latent variables
#
#
#   # debug
#  # obj<- hrmbgobj
#
#   #------------
#
#
#   g<- obj$graph
#   W<- getValue(rdsobj)
#   u<- getRoot(rdsobj)
#   if(is.list(W))
#   {
#     W<- W[[1]]
#     u<- u[1]
#     message("Multiple input sets. Only the first is considered")
#   }
#
#   W_<- base::setdiff(W, u)
#   nW_<- length(W_)
#   q<- c(1:nW_)
#   colIndex<- unlist(sapply(q, function(x) base::setdiff(W_, W_[1:x])))
#   pairsOfSigma<- base::rbind(rep(W_, c(nW_:1)), c(W_, colIndex))
#
#   A<- matrix(0, nrow = ncol(pairsOfSigma), ncol = ecount(g))
#   colnames(A)<- get.edge.attribute(g, "name", E(g))
#   for (i in 1:ncol(pairsOfSigma))
#   {
#     fc<- pairsOfSigma[1,i]
#     sc<- pairsOfSigma[2,i]
#     A[i, ]<- 2*(edge_names_along_path(obj, u, fc, edge_names = FALSE)+
#                 edge_names_along_path(obj, u, sc, edge_names = FALSE)-
#                 edge_names_along_path(obj, fc, sc, edge_names = FALSE))
#     # A[i, ]<- 2*(shortPath2vec(g, u, fc)+
#     #             shortPath2vec(g, u, sc)-
#     #             shortPath2vec(g, fc, sc))
#
#
#   }
#   return(A)
# }


# new version
#' @export
sigma.HRMBG<- function(obj, rdsobj, U_bar, ... )
{
  # the code does not take into account latent variables


  # debug
  # obj<- hrmbgobj

  #------------


  g<- obj$graph
  W<- getValue(rdsobj)
  u<- getRoot(rdsobj)
  if(is.list(W))
  {
    W<- base::setdiff(W[[1]], U_bar)
    u<- u[1]
    message("Multiple input sets. Only the first is considered")
  }
  W<- base::setdiff(W, U_bar)
  W_<- base::setdiff(W, u)
  nW_<- length(W_)
  q<- c(1:nW_)
  colIndex<- unlist(sapply(q, function(x) base::setdiff(W_, W_[1:x])))
  pairsOfSigma<- base::rbind(rep(W_, c(nW_:1)), c(W_, colIndex))

  A<- matrix(0, nrow = ncol(pairsOfSigma), ncol = ecount(g))
  colnames(A)<- get.edge.attribute(g, "name", E(g))
  for (i in 1:ncol(pairsOfSigma))
  {
    fc<- pairsOfSigma[1,i]
    sc<- pairsOfSigma[2,i]
    A[i, ]<- 2*(edge_names_along_path(obj, u, fc, edge_names = FALSE)+
                  edge_names_along_path(obj, u, sc, edge_names = FALSE)-
                  edge_names_along_path(obj, fc, sc, edge_names = FALSE))
    # A[i, ]<- 2*(shortPath2vec(g, u, fc)+
    #             shortPath2vec(g, u, sc)-
    #             shortPath2vec(g, fc, sc))


  }
  return(A)
}

