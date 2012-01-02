\name{INITpickfile}
\alias{INITpickfile}
\title{Initialize a pickfile
}
\description{Initialize a pickfile
}
\usage{
INITpickfile(stas = NULL, src = NULL, WPX = NULL)
}
\arguments{
  \item{stas}{station list
}
  \item{src}{hypocenter location
}
  \item{WPX}{GPIX or PPIX picks from swig
}
}
\details{
Initialize a pickfile with a 
set of picks extracted from swig.
}
\value{list, pickfile
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{EmptyPickfile


}
\examples{

\dontrun{
PF =  INITpickfile(stas=stas, src=NULL, WPX=twpx)

}


}
\keyword{misc}
