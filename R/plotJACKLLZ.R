plotJACKLLZ<-function(hjack, sta, proj=NULL, PLOT=0, PS=FALSE )
{


  ###  if plot = {0,1,2}
#######  plot the output of HiJACK

  YEYE=hjack$Y
  XEYE=hjack$X
  ZEYE=hjack$Z
  
  L1 = as.vector( unlist( lapply(YEYE, "length") ) )
  Lw = which(L1>0)

  YEYEB = vector(mode="list")
  for(i in 1:length(Lw) )
    {
      YEYEB[[i]] = YEYE[[Lw[i]]]

    }
  names( YEYEB) = names(YEYE)[Lw]


 
################################

  L1 = as.vector( unlist( lapply(XEYE, "length") ) )
  Lw = which(L1>0)

  XEYEB = vector(mode="list")
  for(i in 1:length(Lw) )
    {
      XEYEB[[i]] = XEYE[[Lw[i]]]

    }
  names( XEYEB) = names(XEYE)[Lw]
################################
  L1 = as.vector( unlist( lapply(ZEYE, "length") ) )
  Lw = which(L1>0)

  ZEYEB = vector(mode="list")
  for(i in 1:length(Lw) )
    {
      ZEYEB[[i]] = ZEYE[[Lw[i]]]

    }
  names( ZEYEB) = names(ZEYE)[Lw]


##  boxplot.stats(x, coef = 1.5, do.conf = TRUE, do.out = TRUE)
Blat = boxplot(YEYEB, plot=FALSE)
  
  Blon = boxplot(XEYEB, plot=FALSE)
 
  Bz = boxplot(ZEYEB, plot=FALSE)

 ##
  if( PLOT==0 | PLOT==1)
    {
       op <- par(no.readonly = TRUE) 
  if(PS==FALSE) dev.new(width=10, height=8)

  
  par(mfrow=c(3,1))

  
  bxp(Blat, varwidth=TRUE)
  title("Station Influence Lat")

  bxp(Blon, varwidth=TRUE)
  title("Station Influence Lon")

  bxp(Bz, varwidth=TRUE)
  title("Station Influence Z")
par(op)
}


  if( PLOT==0 | PLOT==2)
    {
  if(PS==FALSE)  dev.new(width=5, height=8)

 op <- par(no.readonly = TRUE) 
  par(mfrow=c(3,1))
     

  Gxy = GLOB.XY(  sta$lat   ,sta$lon , proj)
  plot(Gxy$x, Gxy$y, type='n', xlab="km", ylab="km", asp=1 )
  imageINFLUENCE(Blat, sta, proj)
  title("Station Influence Latitude")
 
  plot(Gxy$x, Gxy$y, type='n', xlab="km", ylab="km", asp=1 )
  imageINFLUENCE(Blon, sta, proj)
  title("Station Influence Longitude")
 
  plot(Gxy$x, Gxy$y, type='n', xlab="km", ylab="km", asp=1 )
  imageINFLUENCE(Bz, sta, proj)
  title("Station Influence Depth")

  par(op)

}
  return(list( X=XEYEB, Y=YEYEB, Z=ZEYEB) )
}
