\name{viewCHAC}
\alias{viewCHAC}
\title{View Continuous Data
}
\description{
Scroll through continuous data recorded in the 
field.  Uses a database describing the locations and content
of each file stored on disk.
}
\usage{
viewCHAC(DBnov , gstas, gcomps,sched, stas, buts='GPIX', preFILT=list()
, replot=TRUE , kind=2, Iendian=1, BIGLONG=FALSE)
}
\arguments{
  \item{DBnov}{RSEIS Data Base (output of makeDB)
}
  \item{gstas}{stations to extract
}
  \item{gcomps}{components to extract
}
  \item{sched}{schedule of start times for extraction
}
 \item{stas}{station list
}
\item{buts}{ 
  buttons for swig }

\item{preFILT}{ Pre-Filter traces before plotting. }

 \item{replot}{ logical, TRUE=rerun swig after done click
}
  \item{kind}{ kind of data, 0=nativeR, 1=segy, 2=sac
}
  \item{Iendian}{endian
}
  \item{BIGLONG}{big long or short long
}
}
\details{
These are set up for the CHAC dataset.


}
\value{Graphics, and Side effects
}
\note{
The preFILT argument is the standard way of
assigning filters in RSEIS.  For example,
\verb{
     preFILT = list(ON=TRUE, fl=5 , fh=15, type="BP", proto="BU")
}
will bandpass filter the traces between 5 and 15 Hz.
If the logical ON flag of the filter is turned to FALSE,
the filter is not applied.
   
  

  }

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{makeDB, Mine.seis 
}
\examples{
\dontrun{

##########  set up data base:
fpath = "/home/lees/Site/CHAC/DATA"
fpat = "201111"
DBnov = makeDB(fpath, fpat, kind=2, Iendian=1, BIGLONG=FALSE)
###   get information:
IDB = infoDB(DBnov)

#####  select stations and components:
gstas = IDB$usta[-which(IDB$usta=="CHAC5")]
gcomps = IDB$ucomp[1:3]

####  extra buttons
buts = c("YPIX", "SPEC", "SGRAM", "WLET")
 fsta = "/home/lees/Site/CHAC/staLLZ.txt"
stas = scan(file=fsta,what=list(name="", lat=0, lon=0, z=0))
stas$z = stas$z/1000

###  set schedule
sched  =seq(from=325, to=335, by=1/24)

##########  open 2 windows
X11()
X11()

###  set main window to dev 2
dev.set(2)

###  set pre-filter (needs to be ON=TRUE to work

preFILT = list(ON=TRUE, fl=1/2 , fh=8, type="BP", proto="BU")

viewCHAC( DBnov , gstas, gcomps , sched, stas, buts =buts,
       preFILT = preFILT,kind = 2, Iendian = 1, BIGLONG = FALSE )


}

}
\keyword{misc}
