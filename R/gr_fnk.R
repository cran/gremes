
# don't export

# A function combining the empirical and the parametric stdf
#
# Combines the empirical and the parametric stdf of a HRM tree in
# \eqn{||l_{kn}-l(\theta)||^2}. It is the function which is minimized to obtain the EKS estimators
# of the dependence parameters \eqn{\theta}.
# params A vector of |V|-1 parameters' values
# eksobj An object of class \code{EKS}
# evalPoints A matrix of evaluation points with column names according to the edge names
# stdf_emp The empirical stdf, evaluated at the 'evalPoints', a vector of values
# Ubar the set of non-observed variables
fnk<- function(params, eksobj, evalPoints, stdf_emp, Ubar)
{
  eksobj<- setParams(eksobj, params)

  l<- rep(0, nrow(evalPoints))
  for (i in 1:nrow(evalPoints))
  {
    l[i]<- stdf(eksobj, evalPoints[i, ], Ubar)
  }

  fnk<- crossprod(stdf_emp - l, stdf_emp - l)
  return(fnk)
}
