\name{SavePF}
\alias{SavePF}
\title{Save Pick File Button
}
\description{
Save a pick file from within swig
}
\usage{
SavePF(nh, g)
}
\arguments{
  \item{nh}{RSEIS data list
}
  \item{g}{list of parameters internal to swig
}
}
\details{Uses PFoutput to save a pickfile to disk.
}
\value{Side Effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Pickfile is saved as a native R file with wpx extension
}


\seealso{PFoutput
}
\examples{

\dontrun{
buts = "SavePF"
swig(GH, PADDLAB=buts)

}

}
\keyword{misc}
