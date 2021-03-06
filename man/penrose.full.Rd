% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/penrose.full.R
\name{penrose.full}
\alias{penrose.full}
\title{Plot a full penrose using the penrose() recursive function}
\usage{
penrose.full(core.x = 0, core.y = 0, core.r, final.iter, kite = TRUE,
  dart = TRUE, core.rot = 0, core.sca = 1, cent.rot = 0, cent.sca = 1,
  ticker = 0)
}
\arguments{
\item{core.x}{x coordinate of the true center of the final penrose tiling}

\item{core.y}{y coordinate of the true center of the final penrose tiling}

\item{core.r}{radius of the penrose tiling}

\item{final.iter}{final iteration at which to stop the deflation process}

\item{kite}{boolean on whether to plot kites}

\item{dart}{boolean on whether to plot darts}

\item{core.rot}{numeric degrees to rotate the set of all points by, hinged on the true center}

\item{core.sca}{numeric amount to scale the set of all points by, relative to the true center}

\item{cent.rot}{numeric degrees to rotate the set of all points by, hinged on the center of points}

\item{cent.sca}{numeric amount to scale the set of all points by, relative to the center of points}

\item{ticker}{additional outside parameter for miscellaneous use}

\item{core.x}{numeric x coordinate of true center of the penrose}

\item{core.y}{numeric y coordinate of true center of the penrose}
}
\value{
null, just plots shapes
}
\description{
\code{penrose.full()} sets up a full penrose tiling starting with 
10 kites arranged in a decagon (sun formation)
}
