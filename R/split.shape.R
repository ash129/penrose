#' Split a kite or dart according to penrose construction by deflation
#'
#' \code{split.shape()} takes in x coordinates and y coordinates of a point, as well as the 
#' penrose's true center. You must also specify whether it is a kite or dart. 
#'
#' @param xs numeric vector containing the x coordinates of the points of the shape to be split.
#' 
#' @param ys numeric vector containing the y coordinates of the points of the shape to be split.
#'
#' @param type either "kite" or "dart" to specify the shape
#' 
#' @param core.x numeric x coordinate of true center of the penrose
#'
#' @param core.y numeric y coordinate of true center of the penrose
#'
#' @return list of sets of numeric coordinates for the new shapes from the split
#' 
#' @export

split.shape= function(xs, ys, type, core.x, core.y){
  if(type == "kite"){
    return(split.kite(xs, ys, core.x, core.y))
  } else if (type == "dart") {
    return(split.dart(xs, ys, core.x, core.y))
  }
}

split.kite= function(xs, ys, core.x, core.y){
  
  # the golden ratio
  GR= (1 + sqrt(5))/2
  
  # two new points created by kite trisection
  A.x= (xs[3] + 1/GR * xs[2])/(1+1/GR)
  A.y= (ys[3] + 1/GR * ys[2])/(1+1/GR) 
  B.x= (xs[1] + 1/GR * xs[3])/(1+1/GR)
  B.y= (ys[1] + 1/GR * ys[3])/(1+1/GR) 
  
  # new shapes
  k1.xs= c(B.x, xs[1], xs[2])
  k1.ys= c(B.y, ys[1], ys[2])
  k2.xs= c(B.x, A.x, xs[2])
  k2.ys= c(B.y, A.y, ys[2])
  d1.xs= c(A.x, B.x, xs[3])
  d1.ys= c(A.y, B.y, ys[3])
  
  # angles from core
  k1.ang= simple.angle((k1.xs[1]+k1.xs[3])/2, (k1.ys[1]+k1.ys[3])/2, core.x, core.y)
  k2.ang= simple.angle((k2.xs[1]+k2.xs[3])/2, (k2.ys[1]+k2.ys[3])/2, core.x, core.y)
  d1.ang= simple.angle((d1.xs[1]+d1.xs[3])/2, (d1.ys[1]+d1.ys[3])/2, core.x, core.y)
  
  # get colors from angles
  k1.col= color.shape(k1.ang, type= "kite")
  k2.col= color.shape(k2.ang, type= "kite")
  d1.col= color.shape(d1.ang, type= "dart")
  
  return(list(k1= list(xs= k1.xs, ys= k1.ys, ang= k1.ang, type= "kite", col= k1.col),
              k2= list(xs= k2.xs, ys= k2.ys, ang= k2.ang, type= "kite", col= k2.col),
              d1= list(xs= d1.xs, ys= d1.ys, ang= d1.ang, type= "dart", col= d1.col))
  )
}

split.dart= function(xs, ys, core.x, core.y){
  
  # the golden ratio
  GR= (1 + sqrt(5))/2
  
  # one new point created by dart bisection
  C.x= (xs[2] + 1/GR * xs[3])/(1+1/GR)
  C.y= (ys[2] + 1/GR * ys[3])/(1+1/GR)
  
  # new shapes
  k1.xs= c(xs[1], C.x, xs[3])
  k1.ys= c(ys[1], C.y, ys[3])
  d1.xs= c(C.x, xs[1], xs[2])
  d1.ys= c(C.y, ys[1], ys[2])
  
  # angles from core
  k1.ang= simple.angle((k1.xs[1]+k1.xs[3])/2, (k1.ys[1]+k1.ys[3])/2, core.x, core.y)
  d1.ang= simple.angle((d1.xs[1]+d1.xs[3])/2, (d1.ys[1]+d1.ys[3])/2, core.x, core.y)
  
  # get colors from angles
  k1.col= color.shape(k1.ang, type= "kite")
  d1.col= color.shape(d1.ang, type= "dart")
  
  return(list(k1= list(xs= k1.xs, ys= k1.ys, ang= k1.ang, type= "kite", col= k1.col),
              d1= list(xs= d1.xs, ys= d1.ys, ang= d1.ang, type= "dart", col= d1.col))
  )
}
