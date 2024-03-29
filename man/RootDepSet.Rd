% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gr_RootDepSet.R
\name{RootDepSet}
\alias{RootDepSet}
\title{An empty object of class \code{RootDepSet}}
\usage{
RootDepSet()
}
\value{
Creates an object of class \code{RootDepSet} with two empty slots \code{$value} and \code{$root}.
}
\description{
Creates an object of class \code{RootDepSet} with two empty slots \code{$value} and \code{$root}.
The slots are supposed to be assigned values. The simplest way is using method \code{setRootDepSet}.
Every node with observable variable should be taken as a root.
A subset of nodes is created for every root.
For every root a subset of nodes must be created.
For other examples see Vignette "Subsets and Coordinates".
}
\examples{
rdsobj<- RootDepSet()
rds_values<- list(a=c("a", "b"), b=c("b", "c", "d"))
rds_roots<- c("a", "b")
rdsobj<- setRootDepSet(rdsobj, rds_values, rds_roots)
}
