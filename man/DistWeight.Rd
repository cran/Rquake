\name{DistWeight}
\alias{DistWeight}
\alias{DistWeightLL}
\alias{DistWeightXY}


\title{Distance wheighting
}
\description{
  Distance weighting for non-linear
  earthquake location.
}
\usage{
DistWeight(dist, err, distwt)
DistWeightLL(lat, lon, elat, elon, err, distwt)
DistWeightXY(x, y, ex, ey, err, distwt)
}
\arguments{
  \item{dist}{distance in km
}
  \item{err}{sigma error in seconds
}
  \item{distwt}{distance weighting parameter
  }

  \item{lat}{Latitude}
  \item{lon}{Longitude }
  \item{elat}{Event Latitude }
  \item{elon}{Event Longitude}
  \item{x}{station X(km) }
  \item{y}{station Y(km) }
  \item{ex}{event X (km) }
  \item{ey}{event Y (km) }
  
}
\details{Based  on Lquake scheme from University of Washington.
  If you need to reduce the effect of distance weighting,
  increase distwt.

  Since the hypocenter moves between each iteration,
  the distance weighting is updated.
}
\value{vector of weights
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{


DistWeight(1:10, .4, 20)


}
\keyword{misc}
