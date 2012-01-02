\name{EQXYresid}
\alias{EQXYresid}
\title{Calculate Residuals
}
\description{
given an earthquake hypocenter
and a list of station information, retrieve
the station residuals.
}
\usage{
EQXYresid(XY, vel = list(), h1 = c(0, 0, 0, 0), PLOT = FALSE)
}
\arguments{
  \item{XY}{matrix of station location and arrival times.
}
  \item{vel}{list, RSEIS velocity model 
}
  \item{h1}{hypocenter location, c(x,y,z,t)
}
  \item{PLOT}{logical, TRUE=plot the residuals
}
}
\details{The XY mtrix is in cartesian coordinates, i.e. 
it has been projected into units of km.  Only 1D velocity
models are used at this time.
Only residuals of P and S wave arrivals are estimated.
}
\value{vector, right hand side of the least squares problem.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{travel.time1D,UPdateEQLOC
}
\examples{

\dontrun{
 XY = GLOB.XY(pstas$lat, pstas$lon, proj)
    elcor = rep(0, length(pstas$lat))
    
    DZ = pstas$z - mean(stas$z)
    
    elcor[pstas$phase=="P"] = DZ[pstas$phase=="P"]/v$vp[1]
    elcor[pstas$phase=="S"] = DZ[pstas$phase=="S"]/v$vs[1]
    
    XY$cor = elcor
    XY$phase = pstas$phase
    XY$sec = pstas$sec
    
    eqXY = GLOB.XY(sol[1], sol[2], proj)
    res =  EQXYresid(XY, vel=v , h1=c(eqXY$x, eqXY$y, sol[2],sol[4]) , PLOT=FALSE)
    }


}
\keyword{misc}
