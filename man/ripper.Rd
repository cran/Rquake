\name{ripper}
\alias{ripper}
\title{Rip off Event location information
}
\description{Extract Event location information following Vlocate
}
\usage{
ripper(AQ)
}
\arguments{
  \item{AQ}{event location list
}
}
\details{Extract lat-lon from event locations to track
  intermediate solutions and convergence
}
\value{2 by N matrix, lat-lon
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{plotEQ
}
\examples{

\dontrun{
qtip = ripper(AQ)

}



}
\keyword{misc}
