\name{Rowz2Keep}
\alias{Rowz2Keep}
\title{Rows to Keep for inversion
}
\description{
  Selects which rows in the
  hypocenter determination to keep during non-linear itaerations
  based on robust rsidual elimination.
}
\usage{
Rowz2Keep(Ldat, EQ, G1, RESMAX)
}
\arguments{
  \item{Ldat}{List of station arrivals 
}
  \item{EQ}{Earthquake location
}
  \item{G1}{derivative and travel time estimates
}
  \item{RESMAX}{2-vector for P and S-wave residual maxima
}
}
\details{
  This is a utility used internally.
  
  Residuals greater than the respective maxima
   provided are eliminated in the svd inversion.
  If fewer than 4 remain, the smallest 4 rows are returned.
  
}
\value{Index of good rows
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{XYlocate
}

\keyword{misc}
