% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gr_HRMnetwork.R
\name{MLE2}
\alias{MLE2}
\title{Object of class \code{MLE2}}
\usage{
MLE2(g)
}
\arguments{
\item{g}{must be an \code{igraph} object, a tree.}
}
\value{
Object of class \code{MLE2} with two slots: \code{$graph} containing the graph and \code{$depParams}
containing the edge weights corresponding to the dependence parameters.
The edge weights are initialized with zero values.
}
\description{
It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
the edge weights which are initialized with zero values.
It is intended to be used for estimation of edge weights of models on trees.
Consult Vignettes "Code - Note 2" and "Estimation - Note 2".
}
\examples{
g<- make_tree(8,3, mode="undirected")
g<- set.vertex.attribute(g, "name", V(g), letters[1:8])
mle2<- MLE2(g)
}
