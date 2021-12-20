#' @export
#' @rdname confInt
confInt<- function(obj, ...)
{
  UseMethod("confInt")
}


#' Confidence intervals for pairwise EC estimates
#'
#' Provides confidence intervals for the estimates using the ECE. It is suitable when only pairs are used -
#' pairwise extremal coefficients.
#' @export
#' @rdname confInt
#' @importFrom stats qnorm
#' @param obj is an object of class \code{EKS} which contains the estimates of the edge weights
#' @param evpo is the matrix of evaluation points used in the EC estimator. It should contain only pairs of ones.
#' @param k is the number of upper order statistics. It should be the same as in the estimates of the edge weights.
#' @param level is the level of confidence. default is 0.05.
#' @param ... additional arguments
#' @return A matrix with two columns for the lower and upper bounds for each of the coefficients.
#' @examples
#' seg<- make_tree(3, mode = "undirected")
#' seg<- set.vertex.attribute(seg, "name", V(seg), letters[1:3])
#' X<- matrix(rnorm(5*3), 5, 3) # create the dataset just to create the Tree object
#' colnames(X)<- letters[1:3]
#' tobj<- Tree(seg, X)
#' eks<- EKS(seg)
#' # assign any parameters, in practice these should be the estimates from the ECE
#' eks<- setParams(eks, c(0.1, 0.2, 0.3))
#' tup<- Tuples()
#' x<- rep(1, vcount(seg))
#' names(x)<- getNodesWithData(tobj)
#' coord<- evalPoints(tup, tobj, x)
#' # suppose that the estimates in deParams are obtained for k=100
#' confInt(eks, coord, 100)
confInt.EKS<- function(obj, evpo, k, level = 0.05, ...)
{


  # ## debug
  # obj<- eceobj
  # evpo<- tup
  # U_bar<- c("2", "5")
  # ##-----------------------

  U_bar = NULL

  alpha<- level
  g<- getGraph(obj)
  parms<- getParams(obj)
  sigma<- sigma_L(obj, evpo, U_bar)
  Ldot<- matrix(0, nrow(evpo), length(parms))
  colnames(Ldot)<- names(parms)
  for (j in 1:nrow(evpo))
  {
    Ldot[j,]<- ldParams(obj, evpo[j,])
  }

  M<- solve(t(Ldot)%*% Ldot)%*% (t(Ldot)%*% sigma %*% Ldot)%*% solve(t(Ldot)%*% Ldot)
  #e_names<- get.edge.attribute(g, "name", E(g))
  conf_int<- matrix(0,ecount(g), 2)
  rownames(conf_int)<- names(parms)
  for (e in names(parms))
  {
    conf_int[e,]<- c(parms[e] - stats::qnorm(1-alpha/2)*sqrt(M[e,e]/k),
                     parms[e] + stats::qnorm(1-alpha/2)*sqrt(M[e,e]/k))
  }
  return(conf_int)

}
