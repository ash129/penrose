#' Give 2D Euclidean distance between two points
#'
#' \code{simple.dist()} returns the simple distance between two points
#'
#' @param x1 x coordinate of first point
#' 
#' @param x2 x coordinate of second point
#' 
#' @param y1 y coordinate of first point
#' 
#' @param y2 y coordinate of the second point
#'
#' @return the distance
#' 
#' @export
#' 

simple.dist= function(x1, y1, x2, y2){
  return( sqrt( (x1 - x2)^2 + (y1 - y2)^2 ) ) 
}