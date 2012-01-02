\name{latlonz2wpx}
\alias{latlonz2wpx}
\title{Add Lat-Lon-Z to WPX list
}
\description{
Given an existing list of seismic picks, add
Latitude, Longitude and Elevation
associated with the indicated station.
}
\usage{
latlonz2wpx(twpx, stas)
}
\arguments{
  \item{twpx}{List of picks from swig
}
  \item{stas}{station list
}
}
\details{
The names of the stations 
are matched to the station names int he station file.
}
\value{
Pick file with LLZ added as list members.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{Klocate
}
\examples{

\dontrun{

twpx  = latlonz2wpx(twpx, stas)

}

}
\keyword{misc}
