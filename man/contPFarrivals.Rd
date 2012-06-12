\name{contPFarrivals}
\alias{contPFarrivals}
\title{Contour Pickfile Arrivals
}
\description{
Contour plot of arrival times recorded in
a pickfile list.
}
\usage{
contPFarrivals(PF, stas, proj=NULL, cont=TRUE, POINTS=TRUE, image=FALSE ,
             col=RSEIS::tomo.colors(50), gcol="black",   phase="P", add=TRUE)
}
\arguments{
  \item{PF}{Pickfile list in RSEIS format
}
  \item{stas}{station list
}
  \item{proj}{projection from GEOmap
  }
  \item{cont}{logical, add contour to plot}
  \item{POINTS}{logical, add mark up (stations)  to plot}
  
  
\item{image}{logical, add image to plot
}
 \item{col}{color palette for image}
 \item{gcol}{color for contour lines}

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
library(RSEIS)
data(GH)
data(coso_sta_LLZ)
sta = coso_sta_LLZ
g1 = GH$pickfile


proj = GEOmap::setPROJ(type=2, LAT0 =median(sta$lat) , LON0 = median(sta$lon))


grcol =  grey(seq(from=0.3, to=0.95, length=50))
  contPFarrivals(g1, sta, proj=proj,cont=TRUE, POINTS=TRUE,
                           image=TRUE , col=grcol,     phase="P",
add=FALSE )


}
\keyword{misc}
