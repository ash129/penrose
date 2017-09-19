#' Rotate and scale a set of points from a reference point
#'
#' \code{rotate.set()} returns the angle from one point to another with modifications
#'
#' @param x x coordinates of ending points
#' 
#' @param ax.x x coordinates of starting point
#' 
#' @param y y coordinates of ending points
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

rotate.set= function(xs, ys, ax.x, ax.y, rot= 0, sca= 1){
  
  # number of points
  num.p= length(xs)
  
  # rotate +
  # expand/shrink from true center
  n.xs= sapply(1:num.p, FUN= function(i) {
    rotate(xs[i], ys[i], ax.x, ax.y, rot, sca)$x
  })
  
  n.ys= sapply(1:num.p, FUN= function(i) {
    rotate(xs[i], ys[i], ax.x, ax.y, rot, sca)$y
  })
  
  return(list(x= n.xs, y= n.ys))
}
