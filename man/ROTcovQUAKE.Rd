\name{ROTcovQUAKE}
\alias{ROTcovQUAKE}
\title{Rotated Covariance matrix of Earthquake Location
}
\description{Rotated Covariance matrix of Earthquake Location
}
\usage{
ROTcovQUAKE(XQ, proj, sel = 1, domap = TRUE, cosomap = NA, add = FALSE)
}
\arguments{
  \item{XQ}{List Output of RElocateEQ
}
  \item{proj}{Projection for map
}
  \item{sel}{selected events
}
  \item{domap}{whether to add a map
}
  \item{cosomap}{GEOmap map list
}
  \item{add}{logical, add to plot
}
}
\details{
  This is used in connection with the RElocateEQ function. Uses package rgl.
}
\value{Graphical Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{RElocateEQ, GEOmap
}

\keyword{misc}
