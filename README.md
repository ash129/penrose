
<!-- README.md is generated from README.Rmd. Please edit that file -->
penrose
=======

penrose contains a few R functions to create a P2 Penrose tiling recursively.

Installation
------------

You can install penrose in R with the following commands

``` r
library(devtools)
devtools::install_github("ash129/penrose")
```

Usage
-----

The package is mostly just for calling the main function, penrose.full(). Here is an example:

``` r
library(penrose)

# parameters
core.x= 0 # center x
core.y= 0 # center y
core.r= 500 # radius
final.iter= 7 # number of times to deflate


# no margins
op= par(mar = rep(0, 4))

# set up plot
plot(1, type= "n", xaxt="n", yaxt="n", ann=FALSE, frame.plot= FALSE, 
     xlim=c(-core.r, core.r)*0.85 , 
     ylim=c(-core.r, core.r)*0.85 )

# black backdrop
polygon(c(-core.r, -core.r, core.r, core.r)*2 + core.x, c(core.r, -core.r, -core.r, core.r)*2 + core.y, col= "black")

# penrose
penrose.full(core.x= core.x, core.y= core.y, core.r= core.r, final.iter= final.iter, 
             kite= TRUE, dart= TRUE, core.rot= 0, core.sca= 1, cent.rot= 0, cent.sca= 1)
```

![](README-vignette-1.png)

If you play around with the animation package, you can also make something like the following:

<img src="penrose.anim.gif" style="display: block; margin: auto;" />
