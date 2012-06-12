\name{OnePerSta}
\alias{OnePerSta}
\title{One Phase Pick Per Station
}
\description{
Require only one pick per station of a specified phase.
}
\usage{
OnePerSta(twpx, phase = "Y")
}
\arguments{
  \item{twpx}{WPX list
}
  \item{phase}{character, specific phase
}
}
\details{
This is used to reduce the number of
picks for specific station and phase.  
The purpose is avoid multiple P-wave phases 
for each station in the earthquake location 
routines.

}
\value{WPX list
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
For S-waves there may be multiple
S-wave arrivals, as in the 
case for shear wave splitting.
In that case it is probably best to 
name the phases differently, as in 
S1, S2, for example.  
}


\seealso{cleanWPX, repairWPX
}
\examples{

s1 = RSEIS::setWPX(name="HI", phase="P", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 
s2 = RSEIS::setWPX(name="BYE", phase="P", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 

s3 = RSEIS::catWPX(s1, s2)

s4 = OnePerSta(s3, phase = "P")



}
\keyword{misc}
