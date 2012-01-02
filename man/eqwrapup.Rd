\name{eqwrapup}
\alias{eqwrapup}
\title{Earthquake Wrap Uo
}
\description{Calculate error and summary 
information on earthquake location.
}
\usage{
eqwrapup(Ldat, EQ, vel, distwt=20, lambdareg = 0.0, verbose=FALSE)
}
\arguments{
  \item{Ldat}{List of station arrival times, lat-lon, and uncertainty
}
  \item{EQ}{List of earthquake location: Lat-Lon-z-t
}
  \item{vel}{velocity model
}
  \item{distwt}{distance weight, default=20
  }
  \item{lambdareg}{numeric, regularization parameter (default=0)
  }
  
  \item{verbose}{logical, TRUE=print information to screen
}
}
\details{
Earthquakes are located with a generalized inverse (SVD).
covariance matrix is extracted and 95\% confidence bounds are 
calculated.  Quality factors Q1 and Q1 estimate the 
quality iof the location based on the gap, minimum distance and rms.
}
\value{
List
\item{rms}{Root Mean Square Residual}
    \item{meanres}{Mean Residual}
    \item{sdres}{Standard Dev of residuals}
    \item{sdmean}{Standard error of mean residual}
    \item{sswres}{Sum squared weighted residuals}
    \item{ndf}{Number of Degrees of Freedom}
    \item{sterrx}{km, error in X (East-West)}
    \item{sterry}{km, error in Y (North-South)}
    \item{sterrz}{km, error in Z, (depth)}
    \item{sterrt}{s, Delta-time}
    \item{cov}{covariance matrix (used for error ellipsoids)}
    \item{lam}{lambda}
    \item{gap}{Spatial gap (max subtended angle)}
    \item{herr}{Horizontal error}
    \item{distmin}{Minimum distance to epicenter}
    \item{Q1}{Quality Factor based on Gap and RMS}
    \item{Q2}{Quality factor based on RMS, depth and min-Distance}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  The Damping parameter (lambda) is set to zero.
  In the UW lquake program, lambda is set to 0.02.
}

\seealso{Klocate, Glocate, getGAP
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

names(kuality)




}
\keyword{misc}
