\name{euler_passive}
\alias{euler_passive}
\title{Euler Rotation Angles
}
\description{
  Given three angles return rotation matrix.
}
\usage{
euler_passive(phi, theta, psi)
}
\arguments{
  \item{phi}{angle with x-axis
}
  \item{theta}{angle with y-axis
}
  \item{psi}{angle with z-axis
}
}
\details{Code borrowed from cpp code in package cda. used in rgl.ellipsoid.
}
\value{3 by 3 rotation matrix.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{rgl.ellipsoid
}
\examples{

phi=30*pi/180 ; theta= 20*pi/180; psi = 6*pi/180
rr = euler_passive(phi,theta,psi)



}
\keyword{misc}
