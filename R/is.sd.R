#' Shadow zone
#' 
#' Check if a selected point is in shadow zone
#'
#' @param x    x parameter of the selected point
#' @param y    y parameter of the selected point
#' @param height    topography parameter 
#' @return     T if is in shadow zone; F if not
#' @details    Shadow zone is defined as the region that is within 15 degree
#'             to the highest point nearby.
#' 
#' @export
#' @examples
#' height=topo(100,100); is.sd(10,35,height)

is.sd=function(x,y,height) {
  for(i in c(1:10)){  # 10 is arbitrary
    x1=x-i
    if(x1<1) {x1=x1+100}
    if((height[x1,y]-height[x,y])/i > tan(pi/12)){
      return(T)
    }
  }
  return(F)
}