\name{defaultVEL}
\alias{defaultVEL}
\title{Default Velocity Function
}
\description{Default Velocity Function is returned
  in the event no velocity function is available.
}
\usage{
defaultVEL(kind = 1)
}
\arguments{
  \item{kind}{integer, 1=fuj1, 2=LITHOS
}
}
\details{
  A set of default velocity functions are available.
}
\value{velocity list, P and S waves
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{fuj1.vel
}
\examples{

v = defaultVEL(1)

}
\keyword{misc}
