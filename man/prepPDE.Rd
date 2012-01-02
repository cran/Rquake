\name{prepPDE}
\alias{prepPDE}
\title{Read PDE file
}
\description{Read PDE file output
and prepare for use in Rquake.  Creates
an earthquake catalog.
}
\usage{
prepPDE(fn)
}
\arguments{
  \item{fn}{file name and path
}
}
\details{
Read in a PDE file that has been saved from the
website,
http://earthquake.usgs.gov/earthquakes/eqarchives/epic/epic_global.php
}
\value{List (catalog) of earthquake hypocenters
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
Take care if they change the format.
}

\seealso{
getregionals
}
\examples{

\dontrun{
fpde = '/Users/lees/Site/CHAC/pde_catalog_NOV_2011.txt'
ipde = prepPDE(fpde)
}

}
\keyword{misc}
