\name{plotEQ}
\alias{plotEQ}
\title{Plot Earthquake location
}
\description{Plot Earthquake location
}
\usage{
plotEQ(Ldat, AQ, add = FALSE, prep = FALSE, TIT = "UTM Projected Stations", proj = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{Ldat}{Data list
}
  \item{AQ}{Earthquake solution (location)
}
  \item{add}{logical, TRUE=add to plot
}
  \item{prep}{preparation
}
  \item{TIT}{title
}
  \item{proj}{projection list
}
  \item{xlim}{2-vector, x limits (km)
}
  \item{ylim}{2-vector, y limits (km)
}
}
\details{used internally in RElocateEQ
}
\value{graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}



\seealso{RElocateEQ
}

\keyword{misc}
