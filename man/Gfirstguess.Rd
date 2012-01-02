\name{Gfirstguess}
\alias{Gfirstguess}
\title{First gues from a pick file	
}
\description{
Extract the lat lon from the pick file.
}
\usage{
Gfirstguess(Ldat, type = "first")
}
\arguments{
  \item{Ldat}{
}
  \item{type}{one of "first", "mean", or "median"
}
}
\details{
Either the earliest arrival or the average station is
returned.  Used internally in the earthquake location program
to provide a first guess.
}
\value{
vector, lat, lon, z and tee
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{Klocate
}
\examples{

\dontrun{

twpx  = latlonz2wpx(twpx, stas)

g1 = Gfirstguess(twpx, type = "first")


}

}
\keyword{misc}
