\name{DoRLocate}
\alias{DoRLocate}
\alias{DoUWLocate}

\title{Locate a set of picks in native R format
}
\description{
  This is a script to apply Vlocate.
  After picking arrival times on several
  earthquake events and saving them with swig,
  the saved files can be located sequentially
  with this wrapper function.

}
\usage{
DoRLocate(LF, stas, vel,  params=list(distwt = 100,
          lambdareg=20,
          REG = TRUE,
          WTS = TRUE,
          STOPPING = TRUE,
          tolx = 0.005,
          toly = 0.005,
          tolz = 0.01,  RESMAX = c(4,5),
 maxITER = c(7, 5, 7, 4)
))

DoUWLocate(LF, stas, vel,  params=list(distwt = 100,
          lambdareg=20,
          REG = TRUE,
          WTS = TRUE,
          STOPPING = TRUE,
          tolx = 0.005,
          toly = 0.005,
          tolz = 0.01))



}
\arguments{
  \item{LF}{List of file location to read (output of list.files)
}
  \item{stas}{list, station location: name, lat, lon, z (and correction
    if available)
}
  \item{vel}{list, velocity structure
  }
 \item{params}{list, parameters for Vlocate function
  }
}
\details{Use swig and viewCHAC to pick P and S-wave arrivals, mostly
  via the PickWin button. After an event has been saved to
  disk in a native R format (suffix RDATA), these can be
  loaded and located.

  The UW version is for files already picked and in the ascii-text
  UW-pickfile format.
  
}
\value{list of earthquake location and error ellipsoids.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{Vlocate
}
\examples{

\dontrun{

##########  read in list of pick data
LF = list.files(path="./Detail_picks", pat="RDATA", full=TRUE)

###  read in station location file
fsta = "/home/lees/Site/CHAC/staLLZ.txt"

stas = scan(file=fsta,what=list(name="", lat=0, lon=0, z=0))
stas$z = stas$z/1000

############  set the velocity (this vel is for a geothermal field in Califormia)
data(VELMOD1D)
vel= VELMOD1D


#############   calculate the locations: (use default values)
KAM = DoRLocate(LF, stas, vel)


############  Done with earthquake locations....next pull data out of list

N = length(KAM)

H = list(lat=vector(length=N), lon=vector(length=N),
  z=vector(length=N), date=vector(length=N) ,   gap=vector(length=N),
herr =vector(length=N),
  zerr=vector(length=N),
  qual=vector(length=N))



for(i in 1:length(KAM))
  {
    zip = KAM[[i]]
    
    H$lat[i] = zip$EQ$lat
    H$lon[i] = zip$EQ$lon
    H$z[i] = zip$EQ$z
    H$date[i]  =     dateStamp(zip$EQ$Time)
    
    H$gap[i] = zip$ERR$gap
    H$herr[i] = zip$ERR$herr
    H$zerr[i] = zip$ERR$sterrz
    H$qual[i] = paste(zip$ERR$Q1, zip$ERR$Q2, sep="")
   
  }


data.frame(H)


MLAT = median(stas$lat)
MLON = median(stas$lon)

proj = GEOmap::setPROJ(type=2, LAT0=MLAT, LON0=MLON)

staxy = GEOmap::GLOB.XY(stas$lat, stas$lon, proj)
zq = GEOmap::GLOB.XY(H$lat, H$lon, proj)

plot(c(staxy$x, zq$x) , c(staxy$y, zq$y), type='n', xlab="E, km",
ylab="N, km", asp=1)

points(staxy, pch=6, col='red')

points(zq, pch=8, col='blue')

XYerror.bars(zq$x, zq$y, zq$y-H$herr/2, zq$y+H$herr/2,  zq$x-H$herr/2, zq$x+H$herr/2,
             pch = 1, col =1, barw = 0.05, add = TRUE )

#############  or: plot 95 percent confidence bounds


for(i in 1:length(KAM))
{

  zip = KAM[[i]]
 KOV = zip$ERR$cov[2:4, 2:4]

     eqlipse(zq$x[i], zq$y[i] , KOV,   wcols = c(1,2) , dof=zip$ERR$ndf, border="blue"  )



}

######################################
######################################   UW format data
######################################


setwd("/home/lees/Progs/R_stuff/EARTHQUAKE")


stafile =  "coso_sta.LLZ"

staf = stafile
stas = setstas(stafile )



pdir = "/home/lees/Progs/R_stuff/EARTHQUAKE/pfiles"
LF = list.files(path=pdir, pattern="p$", full.names=TRUE )

KAM = DoUWLocate(LF, stas, vel)




}

}
\keyword{misc}
