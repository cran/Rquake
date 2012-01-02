\name{chak}
\alias{chak}
\title{View Picked Data
}
\description{
Sequentially view data that has been picked previously
using GPIX or PPIX and stored as wpx files.
}
\usage{
chak(DBnov, gstas, gcomps, fn, stas, vel, kind = 2, Iendian = 1, BIGLONG = FALSE)
}
\arguments{
  \item{DBnov}{RSEIS data base
}
  \item{gstas}{stations to extract
}
  \item{gcomps}{components to extract
}
  \item{fn}{wpx file name
}
  \item{stas}{station list
}
 \item{vel}{ 1D velocity model
}
  \item{kind}{ kind of data, 0=nativeR, 1=segy, 2=sac
}
  \item{Iendian}{endian
}
  \item{BIGLONG}{big long or short long
}
}
\details{
Program is used for detailed picking.
A wpx list exists and is read into memory,
then the data is extracted from the disk, plotted with swig.
}
\value{
Pickfile
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{viewCHAC
}
\examples{

\dontrun{
lf = list.files(path="/Users/lees/Site/CHAC/PIX", pattern=".RDATA", full.names=TRUE)

for(i in 1:length(lf))
  {
fn = lf[i]


ret = chak(DBnov , gstas, gcomps , fn, stas, kind=2, Iendian=1, BIGLONG=FALSE)

  if(ret$but == "QUIT") break
  }

}

}
\keyword{misc}
