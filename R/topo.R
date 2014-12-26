#' Create a random topography
#'
#' Create a surface with 5 sand piles randomly distributed on it 
#' 
#' @param x    length of surface
#' @param y    width of surface
#' @return     a random surfacce \code{height} ("Value" section)
#' @details    \code{topo} generates a vector with 0 indicating bare surface, and
#'              2 indicating the height of sand piles. 
#'              Sand piles have a radius of 5m, and a hight of 2m. 
#' @export
#' @examples
#' height=topo(100,100)


topo = function(i,j) {
  
  height=matrix(0, nrow=i, ncol=j)
  pp=sample(10:(i-10), 5, replace=T)
  qq=sample(10:(j-10), 5, replace=T)
  
  for (i in c(1:i)) {
    for(j in c(1:j)){
      if((i-pp[1])^2+(j-qq[1])^2<=99||(i-pp[2])^2+(j-qq[2])^2<=99||(i-pp[3])^2+(j-qq[3])^2<=99||
           (i-pp[4])^2+(j-qq[4])^2<=99||(i-pp[5])^2+(j-qq[5])^2<=99) {
        height[i,j]=5
      } 
    }
  }
  return(height)
}
