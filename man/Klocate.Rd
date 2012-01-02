\name{Klocate}
\alias{Klocate}
\title{Earthquake Hypocenter Location
}
\description{
Earthquake Hypocenter Location
}
\usage{
Klocate(Ldat, sol = c(0, 0, 0, 0), vel=defaultVEL(6), distwt = 20, errtol = c(0.01, 0.01, 0.01), maxit = 20, Lambda = 1, guessdepth = 6, APLOT = FALSE, stas = list(name = "", lat = NA, lon = NA, z = NA))
}
\arguments{
  \item{Ldat}{swig pick list
}
  \item{sol}{vector, initial solution
}
  \item{vel}{velocity list
}
  \item{distwt}{distance weight parameter
}
  \item{errtol}{error tolerance
}
  \item{maxit}{Maximum number of iterations
}
  \item{Lambda}{damping parameter
}
  \item{guessdepth}{initial depth for guess
}
  \item{APLOT}{logical, plot intermediate solutions
}
  \item{stas}{station list
}
}
\details{
Inversion is done with 
SVD.  
}
\value{
Event location in Lat-Lon-Z-T.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
Damped least squares.
}


\seealso{swig, defaultVEL
}
\examples{

\dontrun{

LF = list.files(path=pdir, pattern="p$", full.names=TRUE )

GYPSY2 = vector(mode="list")

for(i in 1:length(LF))
  {
    g1 = getpfile( LF[i], sta=staf ) 
    ##  points(g1$H$lon, g1$H$lat, pch=8, col='red')

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

 
    NEW = Klocate(Ldat, sol=c(MYLOC$y, MYLOC$x, 6, 0) )

    GYPSY2[[i]] = NEW
    
 
  }


}


}
\keyword{misc}
