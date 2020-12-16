\name{getEulers}
\alias{getEulers}
\title{Get Eulers Angles
}
\description{Given a covariance matrix
  calculated with Vlocate, extract euler's angles
  for plotting in rgl
}
\usage{
getEulers(R)
}
\arguments{
  \item{R}{covarince matrix
}
}
\details{Extract the euler angles for plotting an ellipsoid.
psi about X-axis, theta about Y axis, phi about Z-axis.

}
\value{vector, phi theta psi
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Used in conjunction with ROTcovQUAKE
}


\seealso{ROTcovQUAKE
}
\examples{
options(rgl.useNULL = TRUE)
R = matrix( runif(9), ncol=3)

getEulers(R)




}
\keyword{misc}
