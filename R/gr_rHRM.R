#' Generates random sample
#'
#' If the method is called on object of class \code{HRMtree} it generates a random sample from
#' a Markov tree whose every two adjacent nodes are parameterized
#' with a bivariate Huesler-Reiss distribution with parameter the weight associated to the edge connecting the
#' two variables. Markov tree means that the random vector satisfies the global Markov property.
#' See Vignette "Additional functionalities" for further explanation of the distribution from which it is sampled.
#' If the method is called on an object of class \code{HRMBG} then it generates a random sample from a
#' Huesler-Reiss distribution with structured parameter matrix.
#' See Vignette "Additional functionalities" for further explanation of the distribution from which it is sampled.
#' See Vignette "Huesler-Reiss distributions" for the parameterization on block graphs.
#' @rdname rHRM
#' @export
rHRM<- function(obj,...)
{
  UseMethod("rHRM")
}





#' @export
#' @rdname rHRM
#' @param obj Object of class \code{HRMnetwork} or its subclasses such as \code{HRMtree} and \code{HRMBG} or
#' subclasses of these two. If no explicit method exists the method \code{rHRM.HRMnetwork} is called.
#' @param n The size of the sample
#' @param noise TRUE/FALSE indicates whether to include (TRUE) a standard normal noise
#' to all observations in the sample. The default is FALSE.
#' @param lambda is a structured parameter matrix of the Huesler-Reiss distribution. See Vignette
#'  "Huesler-Reiss distributions" parameterization on block graphs.
#' @param ... additional arguments
#' @return A matrix with the generated observations.
#' @examples
#' # create a graph with named vertices
#' g<- graph(c("a", "b", "b","c", "b", "d"), directed = FALSE)
#' # create a HRMtree object
#' myobj<- HRMtree(g)
#' x<- c(0.1,0.2,0.3)
#' myobj<- setParams(myobj, x)
#' # create a dataset
#' mydata<- rHRM(myobj, 1000)
#' mydata_noisy<- rHRM(myobj, 1000, noise=TRUE)
rHRM.HRMnetwork<- function(obj, n, noise = FALSE, ...)
{
  g<- getGraph(obj)
  theta<- getParams(obj)
  NextMethod(graph = g, params = theta, noise = noise, ...)
}


# #' @rdname rHRM
#' @export
#' @importFrom stats rnorm
rHRM.HRMtree<- function(obj, n, noise, graph, params, ...)
{
  # when you run this in the package better change to the C++ function

  X<- data_generation1(graph, params, n, cdistr1)
  if (noise)
  {
    nc<- ncol(X)
    nr<- nrow(X)
    X<- X + abs(matrix(stats::rnorm(nc*nr), nrow = nr, ncol = nc))
  }
  return(X)
}



#' @importFrom stats runif uniroot
data_generation1 <- function(g, theta, n, Fun)
{
  # calls cdistr1, which is based on the transformation y=t/(1-t) for t in (0,1)
  # g is the graph which is assumed to have vertices ids from 1 to nvertices
  # and also it is assumed to have set the ids of the edges too.
  rt<- get.vertex.attribute(g, "name", 1)
  m_pairs<- walking(g, rt)

  X<- matrix(data = c(0), nrow = n, ncol = length(V(g)))
  colnames(X)<- get.vertex.attribute(g, "name", V(g))
  X[,rt]<- -1/log(stats::runif(n))

  for (i in 1:nrow(m_pairs))
  {
    m_pair<- m_pairs[i,]
    theta_index<- get.edge.ids(g, c(m_pair[1],m_pair[2]))
    theta_name<- get.edge.attribute(g, "name", theta_index)
    for (j in 1:n)
    {
      u=runif(1)
      t<- stats::uniroot(Fun,interval=c(0,1),extendInt = "yes",
                  x=X[j,m_pair[1]], th=theta[theta_name], u=u,
                  f.lower = -1, f.upper = 1 )$root
      X[j,m_pair[2]]<- t/(1-t)
    }
  }
  return(X)
}







#' @importFrom stats pnorm
cdistr1<- function(t, x, th, u) {
  # P(Y<=y|X=x) - u = dF(x,y)/dx : f(x) - u when for F is used Huesler Reiss copula and Frechet margins
  # the exact formula used for the HR copula is as follows:
  # C(u,v)=exp{log(u)*pnorm(theta/2+1/theta*log(log(u)/log(v)))
  #           +log(v)*pnorm(theta/2+1/theta*log(log(v)/log(u))}
  # which is taken from the paper of Guidendorf and Segers but with lambda=2*theta.
  y<- t/(1-t)
  # the HR copula in the package R has parameter=2/theta
  a<- th/2+1/th*log(y/x)
  b<- th/2+1/th*log(x/y)
  #u<- runif(1)

  f1<- stats::pnorm(a)*exp(-1/x*(stats::pnorm(a)-1)-1/y*stats::pnorm(b))
  f<- f1 - u
  return(f)
}




#' @rdname rHRM
#' @export
#' @importFrom stats rnorm
#' @importFrom mev rmev
rHRM.HRMBG<- function(obj, lambda, n, noise=FALSE , ...)
{
  # returns a matrix of observations using the Huesler-Reiss distribution from
  # the package 'mev'

  nvert<- vcount(obj$graph)
  X<- mev::rmev(n = n, d = nvert, sigma = lambda, model = "hr")
  colnames(X)<- colnames(lambda)

  if (noise)
  {
    nc<- ncol(X)
    nr<- nrow(X)
    X<- X + abs(matrix(stats::rnorm(nc*nr), nrow = nr, ncol = nc))
  }

  #return(list(obj, lambda, n, noise))
  return(X)
}




