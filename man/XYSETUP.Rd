\name{XYSETUP}
\alias{XYSETUP}
\title{Set up matrix for hypocenter inversion
}
\description{Set up matrix for hypocenter inversion
}
\usage{
XYSETUP(STAS, init, vel)
}
\arguments{
  \item{STAS}{station information from pickfile
}
  \item{init}{initial event location
}
  \item{vel}{list, velocity
}
}
\details{
This sets up the matrix used for nonlinear inversion.
The code does not include
information on the weighting.
Station corrections are included.

The STAS are an internal component of the pickfile.
}
\value{matrix

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
Need scheme for weighting according to errors
in picks and distance weighting.
}

\seealso{setPROJ, GLOB.XY,NLSlocate
}
\examples{
\dontrun{
##  start with the location of the closest station
XY = XYSETUP(STAS, c(STAS$lat[w1],STAS$lon[w1], initz,  STAS$sec[w1]-t0a  ) , vel  )

}

}
\keyword{misc}
