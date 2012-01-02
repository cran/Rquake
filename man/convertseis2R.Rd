\name{convertseis2R}
\alias{convertseis2R}
\title{Convert seismic data
}
\description{Convert seismic data (SAC or SEGY) to native R
}
\usage{
convertseis2R(fn, destdir = ".", kind = 1, Iendian = 1, BIGLONG = FALSE)
}
\arguments{
  \item{fn}{character vector of file names
}
  \item{destdir}{chracter, destination directory
}
  \item{kind}{kind of data (1=SEGY, 2=SAC)
}
  \item{Iendian}{integer, endianess
}
  \item{BIGLONG}{Logical, TRUE=long is 8 bytes
}
}
\details{Program converts a list of files to native R format
for fast loading.
}
\value{Side effects on system
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{Mine.seis
}
\examples{
\dontrun{

lf = list.files(path="/Users/lees/Mydata", pattern="R0*", full.names=TRUE)
convertseis2R(lf, "/Users/lees/MyRdata",  kind = 1, Iendian = 1, BIGLONG = FALSE)
}

}
\keyword{misc}
