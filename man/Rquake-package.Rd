\name{Rquake-package}
\alias{Rquake-package}
\alias{Rquake}
\docType{package}
\title{
Seismic Analysis of Earthquake Hypocenter determination
}
\description{
Packages puts together codes  from RSEIS, GEOmap, RFOC
and others for a complete analysis of hypocenters 
estiamted using seismic data from field campaigns.

}
\details{
\tabular{ll}{
Package: \tab Rquake\cr
Type: \tab Package\cr
Version: \tab 2.0-2\cr
Date: \tab 2012-06-12\cr
License: \tab  GPL \cr
}

Rquake is a package for analaysis of seismic data 
collected continuously, or in trigger mode.
The functions organize other functions from
RSEIS and GEOmap to help researchers 
pick, locate, and store hypocenters
for detailed seismic investigation.
}
\author{
Jonathan M. Lees<jonathan.lees.edu>
Maintainer:Jonathan M. Lees<jonathan.lees.edu>
}

\note{
  \describe{ 
    \item{Functions}{
CONTPF
EQXYresid
INITpickfile
NLSlocate
PFoutput
RQ
SavePF
UPdateEQLOC
XYSETUP
Y2Pphase
chak
contPFarrivals
doAmap
gMAP
getregionals
prepPDE
viewCHAC
  }
}}

\references{
Lee, W.H.K., and S.W. Stewart, Principles and Applications of Microearthquake Networks, Academic Press, New York, 1981.

}

\keyword{ package }
\seealso{
\code{\link[RSEIS:RSEIS-package]{RSEIS}}
}
\examples{

\dontrun{


lf = list.files(path="/Users/lees/Site/CHAC/PIX", pattern=".RDATA", full.names=TRUE)

for(i in 1:length(lf))
  {
fn = lf[i]


ret = chak(DBnov , gstas, gcomps , fn, stas, kind=2, Iendian=1, BIGLONG=FALSE)

  if(ret$but == "QUIT") break
  }




}



}
