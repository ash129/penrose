#' Rotate and scale a point from a reference point
#'
#' \code{rotate()} returns the new point  from one point to another with modifications
#'
#' @param x x coordinate of ending point
#' 
#' @param ax.x x coordinate of starting point
#' 
#' @param y y coordinate of ending point
#' 
#' @param ax.y y coordinate of ending point
#' 
#' @param rot numeric degrees to rotate the ending point
#' 
#' @param sca numeric amount to scale the distance from starting point
#' 
#' @return returns the new x and y point after rotation/scaling
#' 
#' @export
#' 

rotate= function(x, y, ax.x, ax.y, rot=0, sca= 1){
  
  # if it's the same, just throw the same
  if( x == ax.x & y == ax.y){
    return(list(x= x, y= y))
  }
  
  # original distance
  d= simple.dist(x, y, ax.x, ax.y)
  
  # original angle
  ang= simple.angle(x, y, ax.x, ax.y)
  
  # rotation to new angle
  n.ang= ang + rot
  n.ang= n.ang %% 360  
  
  # scale to new distance
  n.d= d * sca
  
  # new coords
  n.x= cos(n.ang * pi / 180) * n.d + ax.x
  n.y= sin(n.ang * pi / 180) * n.d + ax.y
  
  return(list(x= n.x, y= n.y))
}