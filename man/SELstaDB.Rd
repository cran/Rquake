\name{SELstaDB}
\alias{SELstaDB}
\title{Pick stations and components
  interactively
}
\description{Pick stations and components
  interactively.  This is a routine
  used in swig.
}
\usage{
SELstaDB(IDB, sel=1,  newdev = TRUE, STAY = FALSE)
}
\arguments{
  \item{IDB}{list of stations and components
}

  \item{sel}{vector of index to selected traces
}


\item{newdev}{logical, whether
  to create a new device.
}
  \item{STAY}{logical, whether
  to keep device active.
}
}

\value{vector of index to list of stations and components
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{infoDB, makeDB}


\examples{
\dontrun{
fpath = "/Users/lees/Site/CHAC/DATA"
fpat = "201111"
DBnov = makeDB(fpath, fpat, kind=2, Iendian=1, BIGLONG=FALSE)
IDB = infoDB(DBnov)
SELstaDB(IDB)
}
}
\keyword{misc}
