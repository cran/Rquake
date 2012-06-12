imageINFLUENCE<-function(B, sta, proj)
  {
    NEX = 50
    col=tomo.colors(50)
     Mz = match(B$names, sta$name)
     
     gxy = GLOB.XY(  sta$lat[Mz]   ,sta$lon[Mz] , proj)
     
     ex = seq(from=min(gxy$x), to=max(gxy$x), length=NEX)
    why = seq(from=min(gxy$y), to=max(gxy$y), length=NEX)

     mval = as.vector(B$stats[3, ])

     zed  = interp(x=gxy$x , y=gxy$y, z=mval, ex, why, duplicate="mean" )

     image(zed, col=col , add=TRUE)
     
     points(gxy$x, gxy$y)

     text(gxy$x, gxy$y, labels=B$names, pos=3)

   }
