#' Creates an empty object of class \code{Coordinates}
#'
#' It is used for creating the coordinates where the stable tail dependence function (stdf) is evaluated.
#' For instance in the extremal coefficient estimator. For more information about it see
#' Vignettes "Estimation - Note 3" and "Code - Note 3" and "Subsets and Coordinates".
#' @export Coordinates
#' @return An empty object of class \code{coordinates}.
Coordinates<- function()
{
  x<- numeric(0)
  class(x)<- "Coordinates"
  return(x)
}



#' Creates an empty object of class \code{Tuples}
#'
#' It is used for creating the coordinates where the stable tail dependence function (stdf) is evaluated.
#' For instance in the extremal coefficient estimator. For more information about it see
#' Vignettes "Estimation - Note 3" and "Code - Note 3" and "Subsets and Coordinates". It is designed for use with
#' method \code{evalPoints}. See the examples therein.
#' @export Tuples
#' @return An empty object of class \code{Tuples} which is to be used with method \code{evalPoints} to create matrix of
#' coordnates.
#' @examples
#' tup<- Tuples()
#'
#' seg<- make_tree(8,2, mode = "undirected")
#' seg<- set.vertex.attribute(seg, "name", V(seg), letters[1:8])
#' data<- matrix(rnorm(10*8), 10,8)
#' colnames(data)<- letters[1:8]
#' tobj<- Tree(seg, data)
#'
#' x<- rep(1,8)
#' names(x)<- get.vertex.attribute(tobj$graph, "name", V(tobj$graph))
#'
#' ep<- evalPoints(tup, tobj, x)
#' head(ep)
Tuples<- function()
{
  obj<- Coordinates()
  class(obj)<- append(class(obj),"Tuples")
  return(obj)
}




#' Creates an empty object of class \code{Triples}
#'
#' It is used for creating the coordinates where the stable tail dependence function (stdf) is evaluated.
#' For instance in the extremal coefficient estimator. For more information about it see
#' Vignettes "Estimation - Note 3" and "Code - Note 3" and "Subsets and Coordinates". It is designed for use with
#' method \code{evalPoints}. See the examples therein.
#' @export Triples
#' @return An empty object of class \code{Triples} which is to be used with method \code{evalPoints} to create matrix of
#' coordnates.
#' @examples
#' tri<- Triples()
#'
#' seg<- make_tree(8,2, mode = "undirected")
#' seg<- set.vertex.attribute(seg, "name", V(seg), letters[1:8])
#' data<- matrix(rnorm(10*8), 10,8)
#' colnames(data)<- letters[1:8]
#' tobj<- Tree(seg, data)
#'
#' x<- rep(1,8)
#' names(x)<- get.vertex.attribute(tobj$graph, "name", V(tobj$graph))
#' ep<- evalPoints(tri, tobj, x)
#' head(ep)
Triples<- function()
{
  obj<- Coordinates()
  class(obj)<- append(class(obj),"Triples")
  return(obj)
}




#' Creates an empty object of class \code{Quadruples}
#'
#' It is used for creating the coordinates where the stable tail dependence function (stdf) is evaluated.
#' For instance in the extremal coefficient estimator. For more information about it see
#' Vignettes "Estimation - Note 3" and "Code - Note 3" and "Subsets and Coordinates". It is designed for use with
#' method \code{evalPoints}. See the examples therein.
#' @export Quadruples
#' @return An empty object of class \code{Quadruples} which is to be used with method \code{evalPoints} to create matrix of
#' coordnates.
#' @examples
#' quad<- Quadruples()
#' quad
#' seg<- make_tree(8,2, mode = "undirected")
#' seg<- set.vertex.attribute(seg, "name", V(seg), letters[1:8])
#' data<- matrix(rnorm(10*8), 10,8)
#' colnames(data)<- letters[1:8]
#' tobj<- Tree(seg, data)
#'
#' x<- rep(1,8)
#' names(x)<- get.vertex.attribute(tobj$graph, "name", V(tobj$graph))
#' ep<- evalPoints(quad, tobj, x)
#' head(ep)
Quadruples<- function()
{
  obj<- Coordinates()
  class(obj)<- append(class(obj),"Quadruples")
  return(obj)
}




#' Creates an empty object of class \code{Adjacent}
#'
#' It is used for creating the coordinates where the stable tail dependence function (stdf) is evaluated.
#' For instance in the extremal coefficient estimator. For more information about it see
#' Vignettes "Estimation - Note 3" and "Code - Note 3" and "Subsets and Coordinates". It is designed for use with
#' method \code{evalPoints}. See the examples therein.
#' @export Adjacent
#' @return An empty object of class \code{Adjacent} which is to be used with method \code{evalPoints} to create matrix of
#' coordnates.
#' @examples
#' adj<- Adjacent()
#' adj
#' seg<- make_tree(8,2, mode = "undirected")
#' seg<- set.vertex.attribute(seg, "name", V(seg), letters[1:8])
#' data<- matrix(rnorm(10*8), 10,8)
#' colnames(data)<- letters[1:8]
#' tobj<- Tree(seg, data)
#'
#' x<- rep(1,8)
#' names(x)<- get.vertex.attribute(tobj$graph, "name", V(tobj$graph))
#' ep<- evalPoints(adj, tobj, x)
#' head(ep)
Adjacent<- function()
{
  obj<- Coordinates()
  class(obj)<- append(class(obj),"Adjacent")
  return(obj)
}



