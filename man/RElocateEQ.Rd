\name{RElocateEQ}
\alias{RElocateEQ}
\title{Relocate earthquakes stored in UW format
}
\description{Relocate earthquakes stored in UW format
}
\usage{
RElocateEQ(lps, sta, vel, cont = TRUE,
sleep = 0.5, mapfun = NULL, PLOT = TRUE ,
 proj = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{lps}{list of filenames save in UW format
}
  \item{sta}{station location list
}
  \item{vel}{velocity list
}
  \item{cont}{logical, true=add contours
}
  \item{sleep}{pause time between each event in seconds
}
  \item{mapfun}{mapping function
}
  \item{PLOT}{logical, TRUE=plot relocation sequence
}
  \item{proj}{projection list
}
  \item{xlim}{limits on X-direction (km)
}
  \item{ylim}{limits on Y-direction (km)
}
}
\details{
  Cycles through all the events, plots them and contours the
  first arrival times.  Uses P-wave arrivals for plotting.
}
\value{
  list of events with error elliposids and convergence path
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{
  Vlocate
}
\examples{

\dontrun{

cosoqk = RElocateEQ(lps, sta, vel, cont=TRUE,
 mapfun=cosoPLOT, proj=proj, xlim=xlim, ylim=ylim )


}


}
\keyword{misc}
