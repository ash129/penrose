#' Create tweened intervals for animation frames based on arctangents
#'
#' \code{tanh.tween()} creates tweening intervals stopping at integers
#'
#' @param min integer value for first "state"
#' 
#' @param max integer value for last "state"
#' 
#' @param n.interval integer value for number of frames between each state
#' 
#' @param scale numeric parameter for scaling the arctangent function and severity of tweening
#'
#' @return vector of intervals for tweening. 
#' Final number of points will be: ( n.interval * 2 - 1 ) * ( max - min ) 
#' 
#' @export


tanh.tween= function(min= 0, max= 1, n.interval= 5, scale= 5){
  sequence.unit= seq(from= 0, to= 1, length.out= n.interval)
  sequence.unit.tanh= (tanh( (sequence.unit - 0.5) * scale) + 1) / 2
  sequence.forward= c(sapply(min:(max-1), FUN= function(x) x + c(0, sequence.unit.tanh)))
  sequence.full= c(sequence.forward, max, rev(sequence.forward))
  return(sequence.full[-length(sequence.full)])
}
