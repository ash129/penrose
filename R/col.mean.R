#' Give the hex color weighted average of two hex colors 
#'
#' \code{col.mean()} takes two hex colors and two sets of weights to give the average color
#' The direct method gives the rgb average, whereas the indirect method gives the hsv average
#' by finding the shortest angle needed to be traversed by the hue parameter
#'
#' @param col1 first hex color
#' 
#' @param col1 second hex color
#' 
#' @param w1 weight to give first hex color (w1 + w2 = 1)
#'
#' @param w2 weight to give second hex color (w1 + w2 = 1)
#'
#' @param direct if TRUE, will get the rgb average. If FALSE will give the hsv average
#'
#' @return new hex color
#' 
#' @export
#' 

col.mean= function(col1, col2, w1= 0.5, w2= 0.5, direct= TRUE){
  # rgb
  col1.rgb=  data.frame(t(col2rgb(col1)), weight= w1)
  col2.rgb=  data.frame(t(col2rgb(col2)), weight= w2)
  
  new.col= "#000000"
  
  # rgb direct average
  if(direct){
    r.new= col1.rgb$red * w1 + col2.rgb$red * w2
    g.new= col1.rgb$green * w1 + col2.rgb$green * w2
    b.new= col1.rgb$blue * w1 + col2.rgb$blue * w2
    
    new.col= rgb(r.new/255, g.new/255, b.new/255)
    
    # hsv angle average
  } else {
    # hsv (note, h: [0,1)
    col1.hsv= data.frame(t(rgb2hsv(col1.rgb$red, col1.rgb$green, col1.rgb$blue)), weight= w1)
    col2.hsv= data.frame(t(rgb2hsv(col2.rgb$red, col2.rgb$green, col2.rgb$blue)), weight= w2)
    
    col.hsv= rbind(col1.hsv, col2.hsv)
    col.hsv= col.hsv[order(col.hsv$h),]
    
    # which is the shortest route to the other angle?
    if(col.hsv$h[2] - col.hsv$h[1] < 0.495){
      new.h= (col.hsv$h[1] * col.hsv$weight[1] + col.hsv$h[2] * col.hsv$weight[2]) %% 1
      new.col= hsv(new.h, 1, 1)
    } else if (  (col.hsv$h[2] - col.hsv$h[1] >= 0.495) & (col.hsv$h[2] - col.hsv$h[1] <= 0.505)  ){
      #} else if (  (col.hsv$h[2] - col.hsv$h[1] == 0.5) ){
      r.new= col1.rgb$red * w1 + col2.rgb$red * w2
      g.new= col1.rgb$green * w1 + col2.rgb$green * w2
      b.new= col1.rgb$blue * w1 + col2.rgb$blue * w2
      
      new.col= rgb(r.new/255, g.new/255, b.new/255)
    } else     if(col.hsv$h[2] - col.hsv$h[1] > 0.505){
      new.h= ( (col.hsv$h[1]+1) * col.hsv$weight[1] + col.hsv$h[2] * col.hsv$weight[2]) %% 1
      new.col= hsv(new.h, 1, 1)
    }
    
  }
  
  return(new.col)
}