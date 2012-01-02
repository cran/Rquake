\name{getregionals}
\alias{getregionals}
\title{Extract regional events 
}
\description{
Extract regional events from a hypocenter list (catalog)
}
\usage{
getregionals(KAT, Mlat, Mlon, rad = 1000, t1 = 1, t2 = 2)
}
\arguments{
  \item{KAT}{catalog list 
}
  \item{Mlat}{central latitude
}
  \item{Mlon}{central longitude 
}
  \item{rad}{radius (km)
}
  \item{t1}{start time (julian days)
}
  \item{t2}{end  time (julian days)
}
}
\details{
given an earthquake catalog from PDEs, for example,
extract the events that are close to a network in
a given time frame.  The limited data set may be used
to help predict arrival times for known hypocenter 
locations.
}
\value{
Catalog 
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{Mine.seis
}
\examples{


\dontrun{
fpde = '/Users/lees/Site/CHAC/pde_catalog_NOV_2011.txt'

ipde = prepPDE(fpde)
Mlat = median(stas$lat)
Mlon = median(stas$lon)

Lt1 =  JtimL(upf$LOC)
t1 = Lt1-3/(24*6)
t2 = Lt1+3/(24*6)

localeqs = getregionals(KAT, Mlat, Mlon, rad=100 ,  t1=NULL, t2=NULL)

for(i in 1:length(localeqs))
  {
j = localeqs[i]
 at1 = KAT$jsec[j]
        at2 = at1+20/(24*60)
 GH = Mine.seis(at1, at2, DBnov , gstas, gcomps, kind=2, Iendian=1, BIGLONG=FALSE)

hord = which(GH$COMPS=="V")
        
        gret = swig(GH, sel=hord, PADDLAB=buts)
####  save any picks to a file on disc for later use


  }


}


}
\keyword{misc}
