\name{rgl.ellipsoids}
\alias{rgl.ellipsoids}
\title{Add many ellipsoids to an rgl plot.
}

\usage{
rgl.ellipsoids(positions, sizes, angles, col = "red", ...)
}
\arguments{
  \item{positions}{matrix: rgl style positions
}
  \item{sizes}{matrix rgl style sizes
}
  \item{angles}{matrix Euler orientation angles
}
  \item{col}{colors for plotting
}
  \item{\dots}{additional params
}
}
\description{
Create a list of rgl ellipsoids oriented in space.
}

\details{
Each ellipsoid is specified by its position, dimensions, and Euler angles.
}
\value{
  rgl mesh

}
\author{ Baptiste Auguie,
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Calls single rgl.ellipsoid.
}
\seealso{rgl.ellipsoid
}
\keyword{misc}
