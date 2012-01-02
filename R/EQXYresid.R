EQXYresid <-
function(XY, vel=list() , h1=c(0,0,0,0)  , PLOT=FALSE)
  {
###  init is a list with lat lon z and sec (sec is relative to the minute reference)
    if(missing(PLOT))  { PLOT=TRUE }
    
    
    require(RSEIS)

    if(missing(vel))
      {
       
    LITHOS.vel=defaultVEL(2)
       
        vel= LITHOS.vel
      }


    N = length(XY$phase)

    
    XY$r = sqrt((XY$x-h1[1])^2 + (XY$y-h1[2])^2)
    XY$c = rep(0, length(XY$r))
    XY$s = rep(0, length(XY$r))

    XY$c[XY$r>0] = (XY$x[XY$r>0]-h1[1])/XY$r[XY$r>0]
    XY$s[XY$r>0] = (XY$y[XY$r>0]-h1[2])/XY$r[XY$r>0]


  rhs = rep(NA, times=N)
  
    for(i in 1:N )
      {  
        indelta    = XY$r[i]
        
        if(identical(XY$phase[i],"P"))
          {
            TT1 =  travel.time1D(indelta, h1[3], 0, length(vel$zp)   , vel$zp , vel$vp)
          }
        if(identical(XY$phase[i],"S"))
          {
            TT1 =  travel.time1D(indelta, h1[3], 0, length(vel$zs)   , vel$zs , vel$vs)
          }

        
        
        rhs[i] = XY$sec[i]- (h1[4] + TT1$tt+XY$cor[i] ) 

        
      }

 

  return( rhs)

  }
