\name{legitWPX}
\alias{legitWPX}
\title{Legitimate Pix
}
\description{
  Check WPX list for legitimate picks
}
\usage{
legitWPX(twpx, quiet=TRUE)
}
\arguments{
  \item{twpx}{
  }
  \item{quiet}{logical, default=TRUE, FALSE generates an error message
  }
    
}
\details{
  Used internall to test if
  a WPX list has legitimate picks.
  Initially a list is generated with
  NA and 0 values in the place holders.
  If no legitimate picks are added, the list still exists, but the
  picks are bogus, so this routine will return 0.

  
}
\value{integer: 0=not legitimate, 1=legitimate
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
    Currently only the name is tested for all(NA), but
  this might be changed int he future
  for a more sophisticated test.
}


\seealso{PCsaveWPX
}
\examples{

###  test fails
\dontrun{
library(RSEIS)
jk = RSEIS::cleanWPX()
legitWPX(jk)

data(GH)
gwpx = RSEIS::uwpfile2ypx(GH$pickfile)

legitWPX(gwpx)
}

}
\keyword{misc}
