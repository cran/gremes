
#' It retrieves the graph of an object of appropriate class
#'
#' The function is applied on objects of appropriate class with slot 'graph'. It is used instead of the command 'obj_name$graph'.
#' @param obj Object of appropriate class ('Network', 'HRMnetwork')
#' @param ... additional arguments
#' @return  The content of slot \code{$graph}.
#' @rdname getGraph
getGraph<- function(obj,...)
{
  UseMethod("getGraph")
}









#' @rdname getGraph
#' @export
getGraph.default<- function(obj,...)
{
  warning("Default method is called on unrecognised object")
  return(obj)
}



#' @rdname getGraph
#' @export
getGraph.Network<- function(obj,...)
{
  return(obj$graph)
}








#' @rdname getGraph
#' @export
getGraph.HRMnetwork<- function(obj,...)
{
  return(obj$graph)
}













# It retrieves the dataset from an appropriate object
#
# The function is applied on objects of appropriate class with slot 'data'.
# It is used instead of the command 'obj_name$data'.
#
# getData
# obj Object with slot 'data'
# ... Additional arguments#
getData<- function(obj,...)
{
  UseMethod("getData")
}








#' @export
getData.default<- function(obj,...)
{
  warning("Default method is called on unrecognized object")
  return(obj)
}







#' @export
getData.Network<- function(obj,...)
{
  return(obj$data)
}





#' It retrieves the nodes without data
#'
#' It retrieves the value of the slot \code{$noDataNodes}, the set of nodes for which there are no data available
#'  for an object of class \code{Network}.
#' @rdname getNoDataNodes
#' @param obj Object of class \code{Network} or its subclasses \code{Tree, GTree, CovSelectTree, BlockGraph}
#' @param ... additional arguments
#' @return The content of the slot \code{$noDataNodes} of the \code{obj}.
#' @export
getNoDataNodes<- function(obj, ...)
{
  UseMethod("getNoDataNodes")
}


#' @rdname getNoDataNodes
#' @export
getNoDataNodes.default<- function(obj, ...)
{
  return("NA")
}



#' @rdname getNoDataNodes
#' @export
getNoDataNodes.Network<- function(obj, ...)
{
  U_bar<- obj$noDataNodes
  return(U_bar)
}




#' It retrieves the nodes for which data are missing
#'
#' It retrieves the value of the slot \code{$nodesWithData}, the set of nodes for which there are data available
#'  for an object of class \code{Network}.
#' @rdname getNodesWithData
#' @param obj Object of class 'Network' or its subclasses \code{Tree, GTree, CovSelectTree, BlockGraph}
#' @param ... additional arguments
#' @return The content of the slot \code{$nodesWithData} of \code{obj}.
#' @export
getNodesWithData<- function(obj, ...)
{
  UseMethod("getNodesWithData", obj)
}


#' @rdname getNodesWithData
#' @export
getNodesWithData.default<- function(obj, ...)
{
  return(character(0))
}



#' @rdname getNodesWithData
#' @export
getNodesWithData.Network<- function(obj, ...)
{
  U<- obj$nodesWithData

  return(U)
}

#' It retrieves the parameters associated to an HRMnetwork
#'
#' It retrieves the value of the slot \code{$depParams} of an object of class \code{HRMnetwork}.
#' @param obj An object of class 'HRMnetwork'
#' @param ... additional arguments
#' @return The content of the slot \code{$depParams} of \code{obj}.
#' @rdname getParams
getParams<- function(obj, ...)
{
  UseMethod("getParams")
}



#' @rdname getParams
#' @export
getParams.default<- function(obj, ...)
{
  return("Default method is called on object of unknown class")
}


#' @rdname getParams
#' @export
getParams.HRMnetwork<- function(obj, ...)
{
  x<- obj$depParams
  return(x)
}



getValue<- function(obj,...)
{
  UseMethod("getValue")
}








#' @export
getValue.default<- function(obj, ...)
{
  return("Default method called on unrecognized object")
}









#' @export
getValue.RootDepSet<- function(obj, ...)
{
  return(obj$value)
}


#' @export
getValue.RootIndSet<- function(obj, ...)
{
  return(obj$value)
}




getRoot<- function(obj,...)
{
  UseMethod("getRoot")
}






#' @export
getRoot.default<- function(obj, ...)
{
  return("Default method called on unrecognized object")
}




#' @export
getRoot.RootDepSet<- function(obj, ...)
{
  return(obj$root)
}



#' @export
getRoot.RootIndSet<- function(obj, ...)
{
  return(obj$root)
}


