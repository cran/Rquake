\name{doAmap}
\alias{doAmap}
\title{Plot a map of station locations
}
\description{Plot a map of station locations
}
\usage{
doAmap(stas, doproj = TRUE)
}
\arguments{
  \item{stas}{station list
}
  \item{doproj}{logical, if TRUE, project (UTM)
the data so plot is in units of km with the median
lat-lon as the center.
If FALSE, use the lat-lon coordinates.
}
}
\details{The range of the plot is
expanded by 10 percent prior to plotting.
}
\value{list, GEOmap projection
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{gMAP,expandbound,GLOB.XY
}
\examples{
\dontrun{
  fsta = "/Users/lees/Site/CHAC/staLLZ.txt"
stas = scan(file=fsta,what=list(name="", lat=0, lon=0, z=0))
stas$z = stas$z/1000

doAmap(stas, doproj = TRUE)

}

}
\keyword{misc}
