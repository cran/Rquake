\name{LeftjustTime}
\alias{LeftjustTime}
\title{Adjust times relative to 
least minute.
}
\description{Adjust times relative to 
least minute.
}
\usage{
LeftjustTime(g1)
}
\arguments{
  \item{g1}{list with times, yr, jd, hr, mi, sec
}
}
\details{Reutrns the list with the 
times adjusted to the least minimum (left adjusted)
}
\value{list is returned.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\seealso{ recdate
}
\examples{

set.seed(0)

d1  = list(yr=rep(2005, 4), jd=rep(5, 4), hr=rep(6, 4), mi=c(1,1,2,3), sec=runif(4, 0, 60))
LeftjustTime(d1)

}
\keyword{misc}
