
stdf_star<- function(obj, x, k_ratio, ...)
{

  # debug
  # k<- k_ind
  # obj<- tobjR_star

  #-----------------


  if(is.matrix(x))
  {
    rr<- nrow(x)
    nx<- colnames(x)
  }  else {
    rr<- 1
    nx<- names(x)
  }

  U<- getNodesWithData(obj)
  if (length(nx %in% U)!= length(U))
    stop("The names of the coordinates should correspond to the names of the nodes
         with available data")


  l_star<- matrix(0, nrow=rr, ncol = length(k_ratio))

  for(i in 1:rr)
  {
    for (j in 1:length(k_ratio))
    {
      l_star[i,j]<- stdf(obj, x[i,], k_ratio[j])
    }
  }
  l_star<- apply(l_star, 1, mean)
  return(l_star)

}



