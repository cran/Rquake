\name{Y2Pphase}
\alias{Y2Pphase}
\title{
Convert Y-phase to P-phase
}
\description{
Removes extraneous Y-phase from a pick file.
If Ypix were made initially as a rough pick, this
removes them.
}
\usage{
Y2Pphase(twpx)
}
\arguments{
  \item{twpx}{ WPX list
}
}
\details{
Initially many events may be picked using GPIX
button.  These can be removed after the 
P-phases have been determined with PickWin.
}
\value{WPX returned without Y-phases
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{PPIX, GPIX, YPIX, PickWin
}
\examples{
\dontrun{

newwpx = Y2Pphase(twpx)
}

}
\keyword{misc}
