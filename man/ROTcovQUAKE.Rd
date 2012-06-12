\name{ROTcovQUAKE}
\alias{ROTcovQUAKE}
\title{Rotate earthquake covariance matrix
}
\description{Rotate earthquake covariance matrix
}
\usage{
ROTcovQUAKE(XQ, proj, sel = 1, domap = TRUE, cosomap = NA )
}
\arguments{
  \item{XQ}{Event list
}
  \item{proj}{projection list
}
  \item{sel}{selection index
}
  \item{domap}{logical, TRUE=add map
  }
  \item{cosomap}{GEOmap list
  }

  
}
\details{
  Extracts the data from located events and plots the error ellipoids
  using rgl.
}
\value{Graphical Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{RElocateEQ, rgl
}
\examples{

\dontrun{


cosoqk =RElocateEQ(lps, sta, vel, cont=TRUE, mapfun=cosoPLOT )

ROTcovQUAKE(cosoqk, proj)

}

}
\keyword{misc}
