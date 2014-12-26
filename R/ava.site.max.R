#' Maximum avalanching site
#' 
#' Find out if there exists a avalanching site nearby. If not, return (-1,-1). If 
#' it does, return the maximum avalanching site coordinates. 
#'
#' @param x    x parameter of the selected point
#' @param y    y parameter of the selected point
#' @param height    topography parameter 
#' @return     the coordinates of the avalanching site. 
#'             If there are more than one points satisfying avalanching requirement,
#'             return the one with maximum angle to the selected point
#'             If no avalanching site is found, return \code{c(-1,-1)}
#' @details    Avalanching is defined as the region that is equal to or higher than
#'             30 degrees to the highest point nearby.
#' 
#' @export
#' @examples
#' height=topo(100,100); ava.site.max(10,35,height)
#' 
#' 
ava.site.max = function (x,y,height) { 
  if(x-1<1) {x=x+97} # setting dynamic boundaries. Making sure m is within the board
  if(y-1<1) {y=y+97}
  if(x+1>100) {x=x-98}
  if(y+1>100) {y=y-98}
  m=matrix(c(x-1,y,x+1,y,x,y-1,x,y+1),nrow=4,byrow=T)
  max.height=0.1
  max.x=-1
  max.y=-1
  for(i in c(1:4)){
    if ( (height[m[i,1],m[i,2]]-height[x,y]) > tan(pi/6) && height[m[i,1],m[i,2]]> max.height){
      max.x=m[i,1]
      max.y=m[i,2]
      max.height=height[m[i,1],m[i,2]]
    } 
  }
  return(c(max.x,max.y))
}
