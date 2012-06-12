\name{XYlocate}
\alias{XYlocate}
\title{Locate Earthquake with UTM projection
}
\description{
  Non-linear hypocenter location with UTM
  geographical projection.  Used for locating earthquakes in
  local or regional settings.
}
\usage{
XYlocate(Ldat, EQ, vel, maxITER = 10, distwt = 10, lambdareg = 100, FIXZ
= FALSE, REG = TRUE, WTS = TRUE, STOPPING = TRUE, RESMAX = c(.4,.5), tolx = 0.005, toly = 0.005, tolz = 0.01, PLOT = FALSE)
}
\arguments{
  \item{Ldat}{list, must inlude: x,y,err, sec, cor (see details)
}
  \item{EQ}{list, must inlude: x,y,z, t
}
  \item{vel}{list, 1D velocity structure
}
  \item{maxITER}{Maximum number of iterations
}
  \item{distwt}{distance weighting factor
}
  \item{lambdareg}{regularization parameter for damping
}
  \item{FIXZ}{logical, TRUE = fix depth, i.e. only calculate x,y,t
}
  \item{REG}{ logical, TRUE=use regularization
}
  \item{WTS}{logical, TRUE==use weighting
}
  \item{STOPPING}{logical, TRUE=use stopping criteria
  }
  \item{RESMAX}{vector, residual max for P and S, default=c(4,5)
  }
  
  \item{tolx}{numeric, tolerance in km in x direction
}
  \item{toly}{numeric, tolerance in km in y  direction
}
  \item{tolz}{numeric, tolerance in km in z  direction
}
  \item{PLOT}{logical, plot results during iterations
}
}
\details{
  Input pick list must have at x,y,z, sec, cor, err elements
  for each station.  If no station correction is available it is set to
  zero.  If no uncertainty (err) is available, it is set to 0.05 sec.
  Each station must have a finite x-y coordinate and arrival time in
  seconds. Events are located relative to the minute.

  Routine uses the svd in a sequence of linear inversions
  to estimate the nonlinear location.
}
\value{List:
  \item{EQ}{list, Earthquake hypocenter and time}
  \item{its}{number of iterations}
  \item{rms}{rms residual}
  \item{wrms}{wheighted rms residual}
  \item{used}{vector, index of used equations}
  \item{guesses}{list of x,y,z,t intermediate locations when converging }
  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{This routine should be called by a wrapper (Vlocate) that applies the
  algorithm several times and changes parameters based on
  the quality.

  If RESMAX is used and the robust approach yields
  fewer than 4 equations, the best (smallest) four residuals will be
  used to determiine the event location.
  
}


\seealso{Vlocate
}
\examples{

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
    
    proj = setPROJ(type=2, LAT0=MLAT, LON0=MLON)

####   get station X-Y values in km
    XY = GLOB.XY(Ldat$lat, Ldat$lon, proj)
###   add to Ldat list
    Ldat$x = XY$x
    Ldat$y = XY$y
       wstart = which.min(Ldat$sec)



EQ = list(x=XY$x[wstart], y=XY$y[wstart], z=6, t=Ldat$sec[wstart] )

      
 maxITER = 7
###print(EQ)
    AQ = XYlocate(Ldat,EQ,vel, 
      maxITER = maxITER,
      distwt = 1,
      lambdareg =10 ,
      FIXZ = FALSE,
      REG = TRUE,
      WTS = TRUE,
      STOPPING = TRUE,
      RESMAX = c(0.1,0.1),
      tolx =   0.001,
      toly = 0.001 ,
      tolz = 0.5, PLOT=FALSE)

########  update the new location

AXY = XY.GLOB(AQ$EQ$x, AQ$EQ$y, proj)
AQ$EQ$lat = AXY$lat
AQ$EQ$lon = AXY$lon
if(AQ$EQ$lon>180) { AQ$EQ$lon = AQ$EQ$lon-360 }


plot(c(Ldat$x, AQ$EQ$x) , c(Ldat$y,AQ$EQ$y), type='n' , xlab="km",
ylab="km" )

points(Ldat$x, Ldat$y, pch=6)

points(AQ$EQ$x, AQ$EQ$y, pch=8, col='red')

points(EQ$x, EQ$y, pch=4, col='blue')


legend("topright", pch=c(8,4, 6), col=c("red", "blue", "black"),
 legend=c("Final location", "Initial guess", "Station"))


print(AQ)

EQ$x = 10
EQ$y = 2





}
\keyword{misc}
