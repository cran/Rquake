\name{RQ}
\alias{RQ}
\title{Rquake Button
}
\description{
Driver for NLSlocate
}
\usage{
RQ(nh, g, idev = 3)
}
\arguments{
  \item{nh}{RSEIS list 
}
  \item{g}{parameters from swig
}
  \item{idev}{device for plotting
}
}
\details{
Button to be called from 
within swig after picking.
}
\value{
new hypocenter
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{NLSlocate, EQXYresid, XYSETUP, swig,chak
}
\examples{
\dontrun{

 buts = c("GPIX","PPIX", "PickWin", "fspread", "gMAP", "RQ" , "CONTPF")

swig(GH, PADDLAB=buts)
}

}
\keyword{misc}
