\name{CONTPF}
\alias{CONTPF}
\title{Button to Contour Pickfile Arrivals
}
\description{Button to Contour Pickfile Arrivals, used internally in swig.
}
\usage{
CONTPF(nh, g, idev = 3)
}
\arguments{
  \item{nh}{RSEIS list
}
  \item{g}{swig parameters
}
  \item{idev}{device for plotting
}
}
\details{Driver for contPFarrivals
}
\value{Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{contPFarrivals
}
\examples{

\dontrun{

buts = "CONTPF"
swig(GH, PADDLAB=buts)

}

}
\keyword{misc}
