#' Give the hex color to a shape based on hsv coordinates with additional h parameters
#'
#' \code{hsvcol()} takes hsv coordinates and returns the rgb hexcode, 
#' in addition to rotating hue by \code{rot}
#' Requires "ColorPalette" for hsv2rgb()
#'
#' @param ang numeric angle in degrees
#' 
#' @param s numeric saturation for color [0,1]
#' 
#' @param v numeric value for color [0,1]
#'
#' @param rot numeric angle in degrees for rotating the hue parameter
#'
#' @param type either "kite" or "dart" for some additional arbitrary color rules
#'
#' @return list of numeric coordinates for the new shapes from the transform
#' 
#' @export
#' 

hsvcol= function(ang, s=1, v= 1, rot= 0) {
  
  ang= ang + rot
  
  ang= ang %% 360  
  
  rgb= hsv2rgb(ang, s, v) 
  
  return( rgb )
}
