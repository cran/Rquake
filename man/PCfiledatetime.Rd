\name{PCfiledatetime}
\alias{PCfiledatetime}
\title{Create a character string from a date}
\description{
  Create a character string from a date for
  naming unique output files.
}
\usage{
PCfiledatetime(orgtim, tims)
}
\arguments{
  \item{orgtim}{time vector of length 5: c(yr, jd, hr, mi, sec)  }
  \item{tims}{seconds to add to orgtim}
}

\value{
  \item{filename}{character string}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{
library(RSEIS)
data(GH)

g1 = getGHtime(GH)
g2 = unlist(g1)

PCfiledatetime(g2, 1)



}
\keyword{misc}
