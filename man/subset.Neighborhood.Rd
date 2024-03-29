% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gr_subset.R
\name{subset.Neighborhood}
\alias{subset.Neighborhood}
\title{Subsets based on neighborhood}
\usage{
\method{subset}{Neighborhood}(
  obj,
  eta,
  g,
  U_bar = NULL,
  vertices = base::setdiff(get.vertex.attribute(g, "name", V(g)), U_bar),
  matrixForm = FALSE,
  ...
)
}
\arguments{
\item{obj}{Object of class \code{Neighborhood}}

\item{eta}{Order of the neighborhood}

\item{g}{The graph, an \code{igraph} object}

\item{U_bar}{The set of nodes for which data are missing. Default is NULL. If there are nodes for which the
corresponding variables are not observed the set of these nodes should be provided here. This will avoid taking
a variable with unobserved variable to be taken as a root.}

\item{vertices}{Vertex(vertices) for which the neighboring set(s) are derived. Default is all vertices with observed variable.}

\item{matrixForm}{a matrix of ones and zeros: one if the node is in the subset and zero otherwise.}

\item{...}{additional arguments}
}
\value{
An object of class \code{Neighborhood} containing two slots: slot \code{$value} can be a matrix or list. If it
is a matrix, the number of columns is the same as the number of vertices
in the graph. The number of rows is the length of argument \code{vertices}.
If it is a list, the list contains for each of the nodes in \code{vertices}
the neighborhood of order \code{eta}. The slot \code{$root} contains the vertices
for which the neighborhood sets are obtained. If there are nodes with missing data subsets will be created for
roots with available data only. A subset however can contain a node with missing data.
}
\description{
Derives subsets based on neighborhood of a certain order.
The scope is to obtain for every node with observed variable a neibourhood nodes and use these subsets for estimation.
}
\examples{
seg<- graph(c(1,2, 2,3, 2,4, 4,5, 5,6, 5,7), directed = FALSE)
name_stat<- c("paris", "2", "meaux", "melun", "5", "nemours", "sens")
seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
rdsobj<- Neighborhood()
subset(rdsobj, 2, seg)
subset(rdsobj, 2, seg, U_bar = c("melun", "5"), matrixForm = TRUE)
}
