\name{MeanStaDist}
\alias{MeanStaDist}
\title{Mean Station Distance
}
\description{ calculate the mean km distance of a set of Lat-lon pairs
}
\usage{
MeanStaDist(Ldat)
}
\arguments{
  \item{Ldat}{station list with elements of Lat-Lon
}
}
\details{
  Given a list with elements named lat and lon,
  find the mean station distance.
}
\value{scalar
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{setPROJ, GLOB.XY,  dist
}
\examples{

\dontrun{
library(RSEIS)
data(GH)
MeanStaDist(GH$pickfile$STAS)
}

}
\keyword{misc}
