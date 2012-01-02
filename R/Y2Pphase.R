Y2Pphase<-function(twpx)
  {
    ##  convert all Y phases to P
    ##  if there are P and Y picks for the same station, use P, discard Y
    WY  = which(twpx$phase=="Y")
    if(length(WY)<1)  return(twpx)
        WP  = which(twpx$phase=="P")
    if(length(WP)>0)
      {
       mpy =  match(  twpx$name[WY], twpx$name[WP]  )
       wrid = which(!is.na(mpy))
       if(length(wrid)>0)
         {
           twpx =   deleteWPX(twpx,wrid )
           WY  = which(twpx$phase=="Y")
           twpx$phase[WY]  = "P"
         }
     }
    else
      {
        twpx$phase[WY]  = "P"
      }
    
    Awpx = twpx

    
    return(Awpx)
  }
