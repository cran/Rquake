contPFarrivals <- function(PF, stas, proj=NULL, image=FALSE , phase="G", add=TRUE)
  {
    ###  given a pickfile and a station file, contour arrivals
    require(GEOmap)
    if(missing(proj)) proj = NULL

    NEX = 50

    if(length(PF$STAS$sec)<1)
      {
        return(NULL)
      }

    
    pstas  = PF$STAS

    
    w1 = which.min(pstas$sec)

    if(length(w1)<1) { return(NULL) }
    
    wp = which(pstas$phase==phase )

    if(length(wp)<2) { return(NULL) }
    
    stan = pstas$name[wp]
    arr = pstas$sec[wp] - pstas$sec[w1]

   #  print("contPFarrivals: arr")
   #  print(arr)
    
    #  doAmap(stas)
    if(is.null(proj))  proj = setPROJ(type=2, LAT0 =median(stas$lat) , LON0 = median(stas$lon) )

    #############    convert the LATLON of stations to X-Y
    XY = GLOB.XY(stas$lat, stas$lon, proj)
    if(!add) plot(XY, pch=6, xlab="km", ylab="km" , cex=.6 )
   
   
    msta = match(stan, stas$name)

    ex = seq(from=min(XY$x), to=max(XY$x), length=NEX)
    why = seq(from=min(XY$y), to=max(XY$y), length=NEX)
    zed  = interp(x=XY$x[msta] , y=XY$y[msta], z=arr, ex, why)

   
     if(image) image(zed, col=tomo.colors(50) , add=TRUE)

 text(XY, labels=stas$name, pos=3, cex=.6)
 points(XY$x[msta] , XY$y[msta], col='red', cex=1.1)

    
    contour(zed, add=TRUE)
    return(proj)
  }
