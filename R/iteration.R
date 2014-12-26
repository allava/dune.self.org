#' Run interations of sand transportation process
#' 
#' Runs the whole program for j iterations on the topography parameter "height"
#'
#' @param j    Number of iterations    
#' @param height    topography parameter 
#' @return     \code{height} after j iterations
#' @details    The program is conducted by the following steps:
#'             1. Generate topography paramter
#'             2. Select a random site
#'             3. Check if the site is in the shadow zone. If yes, go to next iteration
#'                if not, (and height is not 0), go to next steps
#'             4. Pick up one sand slab and deposit it down wind (along x axis) for 1 grid.
#'             5. Check the erosion site and deposition site if avalanch happens
#'                If avalanch requirements are satisfied, avalanch.
#'             6. Go to next iteration if j requires. 
#'             Sand surface depositional probability is 0.6, and the probability for non-sand 
#'             surface is 0.4
#' 
#' @export
#' @examples
#' height=topo(100,100)
#' iteration(1000,height)
#'

iteration = function(j,height) {
  ps = 0.6 # sand exist at the deposition cell
  pn = 0.4 # sand not existing at the deposition cell
  range.x = 100
  range.y = 100
  
  for (i in c(1:j)) {
    #cat(i," ")
    x0 = sample(1:100,1)
    y0 = sample(1:100,1)
    
    # cat("start with",x0,y0,"\n")
    if(is.sd(x0,y0,height)) {  # cat(" slab is in sd. not moving it.", "\n")
    } else {  # if the starting point is not in shadow zone
      if(height[x0,y0]==0) {
      } else {
        # if slab present 
        # erode(x,y) with p=1 (no vegetation assumed here)
        height[x0,y0] = height[x0,y0]-0.1
        
        # check avalaunch:
        
        max.site=c(-1,-1)
        max.site=ava.site.max(x0,y0,height)
        # speed this part up!! 
        
        while(max.site[1]!=-1 && max.site[2]!=-1 && height[max.site[1],max.site[2]]>0){ # if max.site is replaced, then loop
          ## avalaunch down the steepest angle
          height[max.site[1],max.site[2]] = height[max.site[1],max.site[2]]-0.1
          height[x0,y0] = height[x0,y0] + 0.1
          # cat("max", x0,y0,height[x0,y0], max.site,height[max.site[1],max.site[2]],"\n")
          max.site=ava.site.max(x0,y0,height)
        }
        
        
        ###### deposit(x0,y0)
        x1=x0 + 1  # deposition site: five slab down wind
        y1=y0  
        
        ## dynamic boundary: if out of the field, minus range of field ###
        if(x1>range.x) {x1=x1-range.x}
        
        rn=runif(1)
        # if pn smaller than settlement p, or in shadow zone, then settle
        while(!(is.sd(x1,y1,height)||rn<ifelse(height[x1,y1]>0,ps,pn))){
          rn=runif(1)
          x1=x1+1
          if(x1>range.x) {x1=x1-range.x}
        }
        
        height[x1,y1]=height[x1,y1]+0.1
        
        # cat(x1,y1,height[x1,y1],"\n")
        
        ###############  avalaunch down wind? ###############
        
        min.site=c(-1,-1)
        min.site=ava.site.min(x1,y1,height)
        
        while(min.site[1]!=-1 && min.site[2]!=-1 && height[x1,y1]>0){ # if min.site is replaced, then loop
          ## avalaunch down the steepest angle
          height[min.site[1],min.site[2]]=height[min.site[1],min.site[2]]+0.1 # lowest point +0.1
          height[x1,y1]=height[x1,y1]-0.1
          # cat("min",min.site,height[min.site[1],min.site[2]],height[x1,y1],"\n")
          min.site=ava.site.min(x1,y1,height)
        }
      }   
    }
  } 
  return(height)
}