#' Give angle between two points
#'
#' \code{simple.angle()} returns the angle from one point to another
#'
#' @param x x coordinate of ending point
#' 
#' @param ax.x x coordinate of starting point
#' 
#' @param y y coordinate of ending point
#' 
#' @param ax.y y coordinate of ending point
#'
#' @return the angle in degrees
#' 
#' @export
#' 

simple.angle= function(x, y, ax.x, ax.y){
  x.d= x - ax.x
  y.d= y - ax.y
  
  ang= atan(y.d/x.d) * 180 / pi
  if(x.d < 0){
    ang= 180 + ang
  }
  ang= ang %% 360  
  
  return( ang ) 
}