\name{getresidTT}
\alias{getresidTT}
\title{Travel time residuals
}
\description{Given an earthquake location
  and a set of arrival times,
  return a vector of residuals.
}
\usage{
getresidTT(Ldat, EQ, stas, vel)
}
\arguments{
  \item{Ldat}{List of arrival times
}
  \item{EQ}{List of event location, (lat, lon, z, and time)
}
  \item{stas}{station location list
}
  \item{vel}{list, velocity structure
}
}
\details{1D travel time calculation.
}
\value{vector of residuals
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{travel.time1D
}
\examples{

\dontrun{

#########  LF is a vector of arrival time files
#####  KAM is a set of locations

 load(LF[i])
    
    zip = LeftjustTime(twpx)
    EQ = KAM[[i]]$EQ
    ERR = KAM[[i]]$ERR

    LOC = list(yr=EQ$Time$yr, mo=EQ$Time$mo, dom=EQ$Time$dom, hr=EQ$Time$hr,
       mi=EQ$Time$mi, sec=EQ$Time$sec, jd=EQ$Time$jd, lat=EQ$lat, lon=EQ$lon,
       z=EQ$z, mag=NA,gap=ERR$gap,delta=ERR$distmin,rms=ERR$rms,hozerr=ERR$herr   )

 resids = getresidTT(zip,LOC, stas , vel)

}


}
\keyword{misc}
