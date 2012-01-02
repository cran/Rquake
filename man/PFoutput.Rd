\name{PFoutput}
\alias{PFoutput}
\title{Write a pickfile to disk
}
\description{
Write a pickfile to disk
ina variety of formats.
}
\usage{
PFoutput(PF, stas = NULL, sol = NULL, format = 0)
}
\arguments{
  \item{PF}{Pickfile list from RSEIS
}
  \item{stas}{station list
}
  \item{sol}{solution vector, (lat, lon, z, t0)
}
  \item{format}{integer, 0=all formats, 1=native R, 2=UW, 3=csv)
}
}
\details{
Writes files to disk in local directory.
}
\value{Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
Creates a file name and writes to disk in a variety of formats.

}


\seealso{SavePF, RSEIS
}
\examples{

\dontrun{

PFoutput(nh$PF, stas = g$sta, sol = NULL, format = 0)

}


}
\keyword{misc}
