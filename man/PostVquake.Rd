\name{PostVquake}
\alias{PostVquake}
\title{Plotting error ellipsoids of many events
}
\description{Plotting error ellipsoids of many events
}
\usage{
PostVquake(MANYeq, GX, GY, XY, proj, add=FALSE, ...)
}
\arguments{
  \item{MANYeq}{List of earthquakes following Vlocate
}
  \item{GX}{X-bounds for plot
}
  \item{GY}{Y-bounds for plot
}
  \item{XY}{station locations in km
}
  \item{proj}{projection list
  }
  \item{add}{logical; if TRUE, add to existing plot (DEFAULT=FALSE)
  }
  \item{\dots}{ graphical parameters for plotting (see par)   } 


  
}
\details{Plots the event and the error ellipsoids
}
\value{Graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  This is used to plot many event locations
  and their error ellipsoids
}


\seealso{eqlipse
}

\keyword{misc}
