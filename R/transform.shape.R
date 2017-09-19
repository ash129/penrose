#' Rotate and scale a shape according to parameters
#'
#' \code{transform.shape()} takes in x coordinates and y coordinates of a point, as well as the 
#' penrose's true center. Additional parameters are used to rotate and scale the data. 
#'
#' @param xs numeric vector containing the x coordinates of the points of the shape to be transformed.
#' 
#' @param ys numeric vector containing the y coordinates of the points of the shape to be transformed.
#' 
#' @param core.x numeric x coordinate of true center of the penrose
#'
#' @param core.y numeric y coordinate of true center of the penrose
#'
#' @param core.rot numeric degrees to rotate the set of all points by, hinged on the true center
#' 
#' @param core.sca numeric amount to scale the set of all points by, relative to the true center
#' 
#' @param cent.rot numeric degrees to rotate the set of all points by, hinged on the center of points
#' 
#' @param cent.sca numeric amount to scale the set of all points by, relative to the center of points
#' 
#' @param ticker additional outside parameter for miscellaneous use
#'
#' @return list of numeric coordinates for the new shapes from the transform
#' 
#' @export

transform.shape= function(xs, ys, type, core.x= 0, core.y= 0,
                          core.rot= 0, core.sca= 1, cent.rot= 0, cent.sca= 1, 
                          ticker= 0){
  
  # center of shape
  cent.x= (xs[1] + xs[3])/2
  cent.y= (ys[1] + ys[3])/2
  
  # distance from core to center 
  cent.dist = simple.dist(core.x, core.y, cent.x, cent.y)
  # scaled to [0,1]
  cent.dist.scale= cent.dist / core.r
  # make sure it's within the right bounds
  if(cent.dist.scale > 1){cent.dist.scale= 1 }
  if(cent.dist.scale < 0){cent.dist.scale= 0 }
  
  # angle from core to center (in degrees, [0,360) ) 
  cent.angle = simple.angle(cent.x, cent.y, core.x, core.y)
  
  # get colors from angle
  cent.col= color.shape(cent.angle, type= type)
  
  ## shape transformations
  new.xs= xs
  new.ys= ys
  
  # rotate/scale with core
  core.set= rotate.set(new.xs, new.ys, core.x, core.y, rot= core.rot, sca= core.sca)
  new.xs= core.set$x
  new.ys= core.set$y
  
  # rotate/scale with center of shape
  cent.set= rotate.set(new.xs, new.ys, cent.x, cent.y, rot= cent.rot, sca= cent.sca)
  new.xs= cent.set$x
  new.ys= cent.set$y
  
  #scale dynamically by distance of current shape from core
  
  # fun formula involving cent.dist.scale & a ticker term (0 - 360)
  # scales shapes to be smaller the further they are from the core
  shape.scaler= (cos( (cent.dist.scale * 180 +  ticker) * pi / 180 ) + 1) / 2 
  
  #shape.set= rotate.set(xs, ys, cent.x, cent.y, rot= 0, sca= 1 - cent.dist.scale)
  shape.set= rotate.set(new.xs, new.ys, cent.x, cent.y, rot= 0, sca= shape.scaler)
  new.xs= shape.set$x
  new.ys= shape.set$y
  
  return(list(xs= new.xs, ys= new.ys, ang= cent.angle, type= type, col= cent.col))
}



# rotate and scale a point (x,y) with respect to an axial point (ax.x, ax.y)
# rotate by angle rot
# scale by scalar sca
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

# rotate & scale a list of points based on an axial point
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

# get distance between two 2D points
simple.dist= function(x1, y1, x2, y2){
  return( sqrt( (x1 - x2)^2 + (y1 - y2)^2 ) ) 
}

# get angle between two 2D points in degrees
# from perspective of axial point (ax.x, ax.2)
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