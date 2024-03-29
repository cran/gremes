#' Danube Dataset
#'
#' Dataset of events on Upper Danube. For detailed description of the data please read in the Appendix
#' of the paper below.
#'
#' @name Danube
#' @docType data
#' @author Peiman Asadi, Anthony C. Davison, Sebastian Engelke.
#' @return A matrix whose columns represent variables and rows represent events.
#' @references
#' Peiman Asadi. Anthony C. Davison. Sebastian Engelke. "Extremes on river networks."
#' Ann. Appl. Stat. 9 (4) 2023 - 2050, December 2015. https://doi.org/10.1214/15-AOAS863
#' @keywords data
NULL


#' Seine Dataset
#'
#' Dataset of events on Seine, France. For detailed description of the data please read in the Appendix
#' of the paper below.
#'
#' @name Seine
#' @docType data
#' @author Stefka Asenova, Gildas Mazo, Johan Segers
#' @return A matrix whose columns represent variables and rows represent events.
#' @references
#' Asenova, S., Mazo, G. & Segers, J. Inference on extremal dependence in the domain of attraction of a
#' structured Huesler-Reiss distribution motivated by a Markov tree with latent variables.
#' Extremes 24, 461-500 (2021). https://doi.org/10.1007/s10687-021-00407-5
#' @keywords data
NULL



#' Danube tree
#'
#' Tree corresponding to the Danube network. It consists of 31 nodes.
#'
#' @name DanubeTree
#' @docType data
#' @author Peiman Asadi, Anthony C. Davison, Sebastian Engelke.
#' @return The graph used in the reference below. An \code{igraph} object.
#' @references
#' Peiman Asadi. Anthony C. Davison. Sebastian Engelke. "Extremes on river networks."
#' Ann. Appl. Stat. 9 (4) 2023 - 2050, December 2015. https://doi.org/10.1214/15-AOAS863
#' @keywords data
NULL



#' Danube flow
#'
#' Matrix with information about flow connected nodes.
#'
#' @name DanubeFlow
#' @docType data
#' @author Peiman Asadi, Anthony C. Davison, Sebastian Engelke.
#' @return A symmetric matrix with ones and zeros. An element ij is one if location i is flow connected
#'  with location j.
#' @references
#' Peiman Asadi. Anthony C. Davison. Sebastian Engelke. "Extremes on river networks."
#' Ann. Appl. Stat. 9 (4) 2023 - 2050, December 2015. https://doi.org/10.1214/15-AOAS863
#' @keywords data
NULL

