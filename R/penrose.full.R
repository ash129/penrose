#' Plot a full penrose using the penrose() recursive function
#'
#' \code{penrose.full()} sets up a full penrose tiling starting with 
#' 10 kites arranged in a decagon (sun formation)
#' 
#' @param final.iter final iteration at which to stop the deflation process
#'
#' @param core.x x coordinate of the true center of the final penrose tiling
#' 
#' @param core.y y coordinate of the true center of the final penrose tiling
#'
#' @param core.r radius of the penrose tiling
#'
#' @param kite boolean on whether to plot kites
#' 
#' @param dart boolean on whether to plot darts
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
#' @return null, just plots shapes
#' 
#' @export

penrose.full= function(core.x= 0, core.y= 0, core.r, final.iter, 
                       kite= TRUE, dart= TRUE, core.rot= 0, core.sca= 1, cent.rot= 0, cent.sca= 1, ticker= 0){
  
  # points for full decagon
  decs= sapply(0:9, FUN= function(x) {
    rotate(core.r, 0, core.y, core.x, rot= 36*x, sca=1)
  })
  decs.x= unlist(decs[1,])
  decs.y= unlist(decs[2,])
  
  # order of points used to make full decagon in the correct orientations
  inds.1= c(1, 3, 3, 5, 5, 7, 7, 9, 9, 1)
  inds.2= c(2, 2, 4, 4, 6, 6, 8, 8, 10, 10)
  
  # make decagon
  for(i in 1:10){
    
    ind.1= inds.1[i]
    ind.2= inds.2[i]
    
    penrose.draw(xs= c(decs.x[ind.1], decs.x[ind.2], core.x),
            ys= c(decs.y[ind.1], decs.y[ind.2], core.y), 
            type= "kite", iter= 1, final.iter= final.iter, core.x= core.x, core.y= core.y, core.r= core.r, 
            kite= kite, dart= dart, core.rot= core.rot, core.sca= core.sca, cent.rot= cent.rot, cent.sca= cent.sca, 
            ticker= ticker)
  }
  
}