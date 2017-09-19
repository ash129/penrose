#' The recursive penrose tiling plotting function
#'
#' \code{penrose.draw()} recursively component of the penrose deflation starting with a shape
#' and other parameters
#'
#' @param xs x coordinates of the shape to be deflated
#' 
#' @param ys y coordinates of the shape to be deflated
#' 
#' @param type either "kite" or "dart" to indicate the shape
#'
#' @param iter current iteration of the deflation process. does not need to be an integer
#' 
#' @param final.iter final iteration at which to stop the deflation process
#'
#' @param core.x x coordinate of the true center of the final penrose tiling
#' 
#' @param core.y y coordinate of the true center of the final penrose tiling
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
#' 

penrose.draw= function(xs, ys, type, iter, final.iter, core.x, core.y, core.r, 
                  kite= TRUE, dart= TRUE, core.rot= 0, core.sca= 1, cent.rot= 0, cent.sca= 1, 
                  ticker= 0){
  if(iter + 1 > final.iter){
    
    # original center
    # center of shape
    orig.cent.x= (xs[1] + xs[3])/2
    orig.cent.y= (ys[1] + ys[3])/2
    
    # original angle & color
    orig.ang= simple.angle(orig.cent.x, orig.cent.y, core.x, core.y)
    orig.col= color.shape(orig.ang, s=1, v=1, type= type)
    
    #### make shapes ####
    
    ### old shape features (transform, then split)
    old.points=  transform.shape(xs, ys, type, core.x= core.x, core.y= core.y,
                                 core.rot= core.sca, core.sca= core.sca, cent.rot= cent.rot, cent.sca= cent.sca, 
                                 ticker= ticker)
    old.points=  split.shape(old.points$xs, old.points$ys, old.points$type, 
                             core.x= core.x, core.y= core.y)
    
    # make old angles and colors those of original
    old.points= lapply(1:length(old.points), FUN= function(x) {
      shape= old.points[[x]]
      return(list(xs= shape$xs, ys= shape$ys, ang= orig.ang, type= shape$type, col= orig.col))
    })
    
    
    
    ### new shape features (split, then transform)
    new.points= split.shape(xs, ys, type, 
                            core.x= core.x, core.y= core.y)
    new.points= lapply(1:length(new.points), FUN= function(x) {
      shape= new.points[[x]]
      transform.shape(shape$xs, shape$ys, shape$type, core.x= core.x, core.y= core.y,
                      core.rot= core.sca, core.sca= core.sca, cent.rot= cent.rot, cent.sca= cent.sca, 
                      ticker= ticker) 
    })
    
    ### combine old and new depending on the step
    
    # how much to weight old v. new shapes
    new.weight= final.iter - iter
    old.weight= 1 - new.weight
    
    # based on weights, avg the positions and colors
    comb.points= lapply(1:length(new.points), FUN= function(x) {
      old.shape= old.points[[x]]
      new.shape= new.points[[x]]
      comb.xs= rowSums(cbind(old.shape$xs * old.weight, new.shape$xs * new.weight))
      comb.ys= rowSums(cbind(old.shape$ys * old.weight, new.shape$ys * new.weight))
      comb.col= col.mean(old.shape$col, new.shape$col, old.weight, new.weight)
      return(list(xs= comb.xs, ys= comb.ys, col= comb.col))
    })
    
    ## plot
    sapply(comb.points, FUN= function(shape) {
      polygon(shape$xs, shape$ys, col=shape$col, density= NA, border= NA)
    })
    
  }else{
    # recurse into smaller shapes
    
    if(type == "kite"){
      
      # find new points for dart bisection
      new.points= split.kite(xs= xs, ys= ys, core.x= core.x, core.y= core.y)
      
      penrose.draw(xs= new.points$k1$xs, ys= new.points$k1$ys, 
              type= "kite", iter= iter + 1, final.iter= final.iter, core.x= core.x, core.y= core.y, core.r= core.r, 
              kite= kite, dart= dart, core.rot= core.rot, core.sca= core.sca, cent.rot= cent.rot, cent.sca= cent.sca, 
              ticker= ticker)
      penrose.draw(xs= new.points$k2$xs, ys= new.points$k2$ys, 
              type= "kite", iter= iter + 1, final.iter= final.iter, core.x= core.x, core.y= core.y, core.r= core.r, 
              kite= kite, dart= dart, core.rot= core.rot, core.sca= core.sca, cent.rot= cent.rot, cent.sca= cent.sca, 
              ticker= ticker)
      penrose.draw(xs= new.points$d1$xs, ys= new.points$d1$ys, 
              type= "dart", iter= iter + 1, final.iter= final.iter, core.x= core.x, core.y= core.y, core.r= core.r, 
              kite= kite, dart= dart, core.rot= core.rot, core.sca= core.sca, cent.rot= cent.rot, cent.sca= cent.sca, 
              ticker= ticker)
      
    }
    if(type == "dart"){
      
      # find new points for dart bisection
      new.points= split.dart(xs= xs, ys= ys, core.x= core.x, core.y= core.y)
      
      penrose.draw(xs= new.points$k1$xs, ys= new.points$k1$ys,
              type= "kite", iter= iter + 1, final.iter= final.iter, core.x= core.x, core.y= core.y, core.r= core.r, 
              kite= kite, dart= dart, core.rot= core.rot, core.sca= core.sca, cent.rot= cent.rot, cent.sca= cent.sca, 
              ticker= ticker)
      penrose.draw(xs= new.points$d1$xs, ys= new.points$d1$ys, 
              type= "dart", iter= iter + 1, final.iter= final.iter, core.x= core.x, core.y= core.y, core.r= core.r, 
              kite= kite, dart= dart, core.rot= core.rot, core.sca= core.sca, cent.rot= cent.rot, cent.sca= cent.sca, 
              ticker= ticker)
      
    }
  }
}