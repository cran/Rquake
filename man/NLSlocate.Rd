\name{NLSlocate}
\alias{NLSlocate}
\title{Nonlinear Least Squares Location
}
\description{
Nonlinear Least Squares Location using Gieger's method
}
\usage{
NLSlocate(GH, vel = list(), init = c(0, 0, 0, 0), PLOT = FALSE)
}
\arguments{
  \item{GH}{List, RSEIS
}
  \item{vel}{velocity model
}
  \item{init}{initial guess for event location
}
  \item{PLOT}{logical, TRUE=plot
}
}
\details{
This is an adaptation of non-linear least squares 
inversion for earthquake location.
A residual function is supplied, 
and iterations are performed until the location
is determined.
}
\value{vector, new location
}
\references{
Lee, W.H.K., and S.W. Stewart, 
Principles and Applications of Microearthquake Networks, Academic Press, New York, 1981.

}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
At this stage there are no weighting
mechanisms or code to eliminate 
data that has residuals that are too large.
}


\seealso{swig
}
\examples{

\dontrun{

eqsol = NLSlocate(nh, vel=nh$vel,  PLOT=TRUE )


}


}
\keyword{misc}
