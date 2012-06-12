\name{Vlocate}
\alias{Vlocate}
\title{Hypocenter Determination
}
\description{Hypocenter Determination
  with error checking and adjustments.
}
\usage{
Vlocate(Ldat,EQ,vel, 
                  distwt = 10,
                  lambdareg =100,
                  REG = TRUE,
                  WTS = TRUE,
                  STOPPING = TRUE,
                  tolx = 0.1,
                  toly = 0.1,
                  tolz = 0.5,
                  RESMAX = c(.4,.5),
                  maxITER = c(7, 5, 7, 4),
                  PLOT=FALSE)
}
\arguments{
    \item{Ldat}{list, must inlude: lat, lon ,err, sec, cor (see details)
}
  \item{EQ}{list, must inlude: lat,lon,z, t
}
  \item{vel}{list, 1D velocity structure
}
 
  \item{distwt}{distance weighting factor
}
  \item{lambdareg}{regularization parameter for damping
}
  
  \item{REG}{ logical, TRUE=use regularization
}
  \item{WTS}{logical, TRUE==use weighting
}
  \item{STOPPING}{logical, TRUE=use stopping criteria
}
  \item{tolx}{numeric, tolerance in km in x direction
}
  \item{toly}{numeric, tolerance in km in y  direction
}
  \item{tolz}{numeric, tolerance in km in z  direction
  }
 \item{RESMAX}{vector, residual max for P and S, default=c(4,5)
  }
  
  \item{maxITER}{vector, Maximum number of iterations
    for each section of the location routine, default=c(7,5,7,4)
}
  \item{PLOT}{logical, plot results during iterations
}
}
\details{This is a wrapper for XYlocate, only here the lat-lon
  of the stations is passed and the code does the
  projection internally.
  
  There are 3 main loops, each controled by differing input params: first event is located only in XY
  keeping the depth fixed (7 iterations).  Then an initial free solution
  is estimated using
  robust elimination of residual based on RESMAX (5 iterations).
  Finally a set of 7 iterations is applied providing the final
  estimate, along with error bars, elliposids, etc.

  In the event no good solution is derived, the regularization parameter
  is doubled and a loop with 4 iterations is applied, and the result
  returned.
  
}
\value{list:
  \item{EQ}{Hypocenter lcoation}
  \item{ERR}{Error Analysis}
  \item{its}{number of iteration}
  \item{Ksolutions}{list of matrices, each with intermediate x,y,z,t locations}
  
}
\references{Lee and Stewart
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{The schedule may be adjusted by
  duplicating this function and changing the
  maxit parameters.
}


\seealso{XYlocate, Klocate, DoRLocate
}
\examples{
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


wstart = which.min(Ldat$sec)
        EQ = list(lat=Ldat$lat[wstart], lon=Ldat$lon[wstart], z=6, t=Ldat$sec[wstart] )
      

  AQ = Vlocate(Ldat,EQ,vel, 
      distwt = 10,
      lambdareg =100 ,
      REG = TRUE,
      WTS = TRUE,
      STOPPING = TRUE,
      tolx =   0.01,
      toly = 0.01 ,
      tolz = 0.05, maxITER = c(7,5,7,4) , RESMAX = c(0.1, 0.1),  PLOT=FALSE)



}
\keyword{misc}
