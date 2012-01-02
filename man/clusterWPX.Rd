\name{clusterWPX}
\alias{clusterWPX}
\title{Cluster Analysis of Picks
}
\description{
Given a pick file in WPX format, break the
picks apart clustered accoring to 
single link cluster analysis.
}
\usage{
clusterWPX(twpx, tol = 200, PLOT = FALSE)
}
\arguments{
  \item{twpx}{WPX list
}
  \item{tol}{tolerance in seconds - all
pick distances less than tol will be set to zero 
to force these to be associated.
}
  \item{PLOT}{logical, if TRUE, add verbose plotting 
}
}
\details{
If there is not significant separation of picks,
only one cluster is returned.  
To avoid spurious clusters, increase the tolerance.
}
\value{
list of WPX lists
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
Cluster depends on what one considers a cluster.
}


\seealso{
addWPX,	catWPX,	checkWPX,cleanWPX,saveWPX, setWPX, repairWPX
}
\examples{

\dontrun{
s1 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 

s2 = setWPX(name="HI", yr=2011, jd=231, hr=5, mi=2, sec = runif(5)) 


s3 = catWPX(s1,s2)

L3 = clusterWPX(s3)

}

}
\keyword{misc}
