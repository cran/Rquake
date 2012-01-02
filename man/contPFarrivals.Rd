\name{contPFarrivals}
\alias{contPFarrivals}
\title{Contour Pickfile Arrivals
}
\description{
Contour plot of arrival times recorded in
a pickfile list.
}
\usage{
contPFarrivals(PF, stas, proj = NULL, image = FALSE,  phase="G", add = TRUE)
}
\arguments{
  \item{PF}{Pickfile list in RSEIS format
}
  \item{stas}{station list
}
  \item{proj}{projection from GEOmap
}
\item{image}{logical, add image to plot
}
\item{phase}{character,  phase to contour
}
\item{add}{logical, TRUE=add to existing plot
}
}
\details{Contours the arrival time.
The earliest arrival is subtracted from each
time pick.  Uses only the phase indicated and there can be only
one phase per station - default is earliest at each station. 
}
\value{Graphical Side Effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{doAmap
}
\examples{
\dontrun{

contPFarrivals(nh$pickfile, nh$sta, proj=NULL)

}

}
\keyword{misc}
