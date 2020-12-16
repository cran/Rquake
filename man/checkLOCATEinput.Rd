\name{checkLOCATEinput}
\alias{checkLOCATEinput}
\title{Check Location data
}
\description{
Check to see if location data has the minimally correct list components.
}
\usage{
checkLOCATEinput(Ldat, EQ, vel = NULL)
}
\arguments{
  \item{Ldat}{list, must inlude: x,y,err, sec, cor (see details)
}
  \item{EQ}{list, must inlude: x,y,z, t
}
  \item{vel}{list, 1D velocity structure
}
}
\details{
  Input pick list must have at x,y,z, sec, cor, err elements
  for each station.
  
}
\value{logical: FALSE mean problem with data
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{XYlocate
}
\examples{

\dontrun{
library(RSEIS)
data(GH)

g1 = GH$pickfile
data(VELMOD1D)
vel= VELMOD1D



 w1 = which(!is.na(g1$STAS$lat))
         sec = g1$STAS$sec[w1]

         N = length(sec)
         Ldat =    list(
           name = g1$STAS$name[w1],
           sec = g1$STAS$sec[w1],
           phase = g1$STAS$phase[w1],
           lat=g1$STAS$lat[w1],
           lon = g1$STAS$lon[w1],
           z = g1$STAS$z[w1],
           err= g1$STAS$err[w1],
           yr = rep(g1$LOC$yr , times=N),
           jd = rep(g1$LOC$jd, times=N),
           mo = rep(g1$LOC$mo, times=N),
           dom = rep(g1$LOC$dom, times=N),
           hr =rep( g1$LOC$hr, times=N),
           mi = rep(g1$LOC$mi, times=N) )

 MLAT = median(Ldat$lat)
    MLON = median(Ldat$lon)
    
    proj = GEOmap::setPROJ(type=2, LAT0=MLAT, LON0=MLON)

####   get station X-Y values in km
    XY = GEOmap::GLOB.XY(Ldat$lat, Ldat$lon, proj)
###   add to Ldat list
    Ldat$x = XY$x
    Ldat$y = XY$y
       wstart = which.min(Ldat$sec)



EQ = list(x=XY$x[wstart], y=XY$y[wstart], z=6, t=Ldat$sec[wstart] )

checkLOCATEinput(Ldat, EQ)
}
}
\keyword{misc}
