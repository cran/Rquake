PostVquake<-function(jimmy, GX, GY, XY, proj)
  {

    zcols = c('red', 'blue', 'purple', 'cyan' )

  
    ZEDS = matrix(ncol=3, nrow=length(jimmy))
    

    for(i in 1:length(jimmy))
      {
        zip = jimmy[[i]]$Ksolutions
        ZEDS[i, ] = c(jimmy[[i]]$EQ$lat,jimmy[[i]]$EQ$lon, jimmy[[i]]$EQ$z ) 
        
        for(iz in 1:length(zip))
          {
            miz =  zip[[iz]]
            points(miz[,1], miz[,2], col=zcols[iz], pch=iz) 
            lines(miz[,1], miz[,2], col=zcols[iz] )

          }

      }



    plot(GX, GY, type='n', xlab="km" , ylab="km" , asp=1)
    points(XY, pch=6)
    ZEXY = GLOB.XY(ZEDS[,1], ZEDS[,2], proj)
    points(ZEXY, pch=8, col='red')



    for(i in 1:length(jimmy))
      {
        KOV =  jimmy[[i]]$ERR$cov[2:4, 2:4]
        ndf = jimmy[[i]]$ERR$ndf
        
        eqlipse(ZEXY$x[i], ZEXY$y[i] , KOV,   wcols = c(1,2) , dof=ndf, border="blue"  )
      }
    
    

  }


######################################















