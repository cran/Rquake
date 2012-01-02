\name{ReSet}
\alias{ReSet}
\title{Button to reset the choices of station and component 
}
\description{Button to reset the choices of station and component in swig and Mine.seis
}
\usage{
ReSet(nh, g)
}
\arguments{
  \item{nh}{RSEIS list
}
  \item{g}{swig parameters
}
 
}
\details{Driver for SELstaDB
}
\value{Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{SELstaDB, Mine.seis
}
\examples{

\dontrun{

buts = "ReSet"
swig(GH, PADDLAB=buts)

}

}
\keyword{misc}
