\name{UPdateEQLOC}
\alias{UPdateEQLOC}
\title{Update an Earthquake location
}
\description{Update an Earthquake location following
a relocation.
}
\usage{
UPdateEQLOC(PF, sol, vel, stas = NULL)
}
\arguments{
  \item{PF}{Pickfile List
}
  \item{sol}{solution vector (lat, lon, z, t0)
}
 \item{vel}{ 1D velocity model
}
  \item{stas}{station list (name, lat, lon, z)
}
}
\details{After re-picking or changing the model or the
station corrections, update the event location in
the pickfile.

}
\value{Pickfile List
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{EQXYresid, NLSlocate,PFoutput
}
\examples{

\dontrun{
upf = UPdateEQLOC(PF,  sol, stas=stas)


}


}
\keyword{misc}
