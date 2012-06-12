\name{BLACKJACK}
\alias{BLACKJACK}
\title{Jackknife earthquake location
}
\description{Perform jackknife on earthquake location
  by eliminating stations.
}
\usage{
BLACKJACK(Ldat, vel)
}
\arguments{
  \item{Ldat}{event list
  }
  \item{vel}{velocity model
} 
}
\details{stations are eliminated, not rows?
}
\value{event list with pseudo values
}
\references{
  Iversen, E. S., and J. M. Lees (1996), A statistical technique for validating velocity models, Bull. Seismol. Soc. Am. 86(6), 1853-1862.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  events are located with P and S-wave arrivals,
  but code here should eliminate just stations.
}


\seealso{Vlocate,HiJACK, plotJACKLLZ
}
\examples{


\dontrun{
COSOjack = HiJACK(lps, sta)


plotJACKLLZ(COSOjack, sta, proj)

}

}
\keyword{misc}
