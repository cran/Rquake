\name{viewseis}
\alias{viewseis}
\title{View Continuous Data
}
\description{
Scroll through continuous data recorded in the 
field.  Uses a database describing the locations and content
of each file stored on disk.
}
\usage{
viewseis(DBnov , gstas, gcomps,sched, stas, buts='GPIX', replot=TRUE , kind=0, Iendian=1, BIGLONG=FALSE)
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
These are set up for the seis dataset
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
viewseis( DBnov , gstas, gcomps , sched, kind=2, Iendian=1, BIGLONG=FALSE)
}

}
\keyword{misc}
