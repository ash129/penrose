#' Give the hex color to a shape based on hsv coordinates and additional rules
#'
#' \code{color.shape()} takes hsv coordinates and returns the rgb hexcode, 
#' but with some additional rules, for artistic purposes
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

color.shape= function(ang, s= 1, v= 1, rot= 0, type= "kite"){
  
  # additional rotation based on shape
  type.rot=0
  if(type == "dart"){
    type.rot= 180
  }
  
  return( hsvcol(ang, s= s, v= v, rot= rot + type.rot) )
}


# gives rgb colors from hsv color angle in degrees
# rot modifies color angle
hsvcol= function(ang, s=1, v= 1, rot= 0) {
  
  ang= ang + rot
  
  ang= ang %% 360  
  
  rgb= hsv2rgb(ang, s, v) 
  
  return( rgb )
}
