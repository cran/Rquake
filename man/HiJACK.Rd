\name{HiJACK}
\alias{HiJACK}
\title{Jackknife a list of events
}
\description{Jackknife a list of events
}
\usage{
HiJACK(lps, sta, vel)
}
\arguments{
  \item{lps}{vector of filenames with UW events
}
  \item{sta}{staiton list
}
 \item{vel}{velocity list
}


}
\details{Driver for BLACKJACK
}
\value{jackknife pseudovalues for each event
}
\references{
  Iversen, E. S., and J. M. Lees (1996), A statistical technique for
  validating velocity models,
  Bull. Seismol. Soc. Am. 86(6), 1853-1862.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{BLACKJACK
}
\examples{


\dontrun{
COSOjack = HiJACK(lps, sta)


plotJACKLLZ(COSOjack, sta, proj)

}



}
\keyword{misc}
