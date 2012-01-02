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
viewCHAC(DBnov , gstas, gcomps,sched, stas, buts='GPIX', replot=TRUE , kind=2, Iendian=1, BIGLONG=FALSE)
}
\arguments{
  \item{DBnov}{RSEIS Data Base
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
These are set up for the CHAC dataset
}
\value{Graphics, and Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{makeDB, Mine.seis 
}
\examples{
\dontrun{
sched  =seq(from=325, to=335, by=1/24)
viewCHAC( DBnov , gstas, gcomps , sched)
}

}
\keyword{misc}
