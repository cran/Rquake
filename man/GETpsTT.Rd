\name{GETpsTT}
\alias{GETpsTT}
\title{Get Pand S travel times and derivatives
}
\description{Get Pand S travel times and derivatives
}
\usage{
GETpsTT(phase, eqz = 6, staz = 0, delx = 1, dely = 1, deltadis = 6, vel)
}
\arguments{
  \item{phase}{character vector, phase
}
  \item{eqz}{event depth
}
  \item{staz}{station elevation
}
  \item{delx}{km, delta X
}
  \item{dely}{km, delta Y
}
  \item{deltadis}{km, distance
}
  \item{vel}{velocity models (P and S)
}
}
\details{
  Creates a vector of travel times, and a matrix
  and derivatives used for inversion.
}
\value{list:
  \item{TT}{travel time vector}
  \item{Derivs}{matrix of derivatives, dtdx, dtdy, dtdz }
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{many.time1D
}
\examples{

library(RSEIS)
library(GEOmap)


data(GH)



data(VELMOD1D)
vel = VELMOD1D


p1 = GH$pickfile$STAS


loc = GH$pickfile$LOC


proj  = setPROJ(type = 2, LAT0 =loc$lat, LON0 =  loc$lon)


XYsta = GLOB.XY(p1$lat, p1$lon, proj)
XYq =   GLOB.XY(loc$lat, loc$lon, proj)

delx = XYq$x-XYsta$x
dely = XYq$y-XYsta$y
dists = sqrt(delx^2+dely^2)

G1 = GETpsTT(p1$phase, eqz=loc$z, staz=0, delx=delx, dely=dely,  deltadis=dists , vel)


  

}
\keyword{misc}
