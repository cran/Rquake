\name{eqlipse}
\alias{eqlipse}
\title{Error Elipse for Hypocenter Location
}
\description{Error Elipse for Hypocenter Location
}
\usage{
eqlipse(x, y, cov, wcols = c(1, 2), dof = 2, pct=0.05, ...)
}
\arguments{
  \item{x}{X-location for drawing
}
  \item{y}{Y-location for drawing
}
  \item{cov}{matrix, 3 by 3 Covariance matrix 
}
\item{wcols}{vector, which columns to extract from cov,
  see details.
}
  \item{dof}{Degrees of Freedom for 95 percent confidence
  }
  \item{pct}{ Percent used for 2-sided confidence bounds, default=0.05 }
  \item{\dots}{graphical parameters, par} 
}
\details{
  The 3 by 3 matrix is supplied and
  a 2 by 2 matrix is subtracted depending on
  which components are being drawn.
  For X-Y projections, use  wcols=c(1,2).
  For vertical cross sections, rotate the
  cov matrix and then extract the columns.
}
\value{Side effects, graphical
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{eqwrapup
}
\examples{



require(RSEIS)
data(GH)
data(wu_coso.vel)
vel = wu_coso.vel


gpf = GH$pickfile

w1 = which(gpf$STAS$phase=="P" | gpf$STAS$phase=="S" )

N = length(w1)

 Ldat =    list(
      name = gpf$STAS$name[w1],
      sec = gpf$STAS$sec[w1],
      phase = gpf$STAS$phase[w1],
      lat=gpf$STAS$lat[w1],
      lon = gpf$STAS$lon[w1],
      z = gpf$STAS$z[w1],
      err= gpf$STAS$err[w1],
      yr = rep(gpf$LOC$yr , times=N),
      jd = rep(gpf$LOC$jd, times=N),
      mo = rep(gpf$LOC$mo, times=N),
      dom = rep(gpf$LOC$dom, times=N),
      hr =rep( gpf$LOC$hr, times=N),
      mi = rep(gpf$LOC$mi, times=N) )

EQ = GH$pickfile$LOC

EQ$t = EQ$sec

kuality = eqwrapup(Ldat, EQ, vel, distwt = 20, verbose = TRUE )


 MLAT = median(Ldat$lat)
  MLON = median(Ldat$lon)
  proj = GEOmap::setPROJ(type=2, LAT0=MLAT, LON0=MLON)

  XYSTAS = GEOmap::GLOB.XY(Ldat$lat,  Ldat$lon , proj)


 eqxy = GEOmap::GLOB.XY(EQ$lat, EQ$lon, proj)


plot(range(c(XYSTAS$x, eqxy$x)), range(c(XYSTAS$y, eqxy$y)), type='n', asp=1, xlab="km", ylab="km" )
points(XYSTAS$x, XYSTAS$y, pch=6)
points(eqxy$x, eqxy$y, pch=8, col='red')


KOV = kuality$cov[2:4, 2:4]

eqlipse(eqxy$x, eqxy$y , KOV,   wcols = c(1,2) , dof=kuality$ndf, border="blue"  )

}
\keyword{misc}
