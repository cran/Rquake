RQ <- function(nh, g, idev=3)
  {   ####  relocation button for swig
    require(minpack.lm)
    require(RSEIS)
    require(GEOmap)

    if(is.null(nh$sta))
      {
        print("no station file")
        invisible(list(global.vars=g))
      }

    if(is.null(nh$vel))
      {
        data(fuj1.vel)
        nh$vel = fuj1.vel
      }

    if(is.null(nh$pickfile))
      {
        print("RQ: no pickfile....converting")
        if(is.null(g$WPX) )invisible(list(global.vars=g))
        twpx = g$WPX
         twpx = Y2Pphase(twpx)

        
        nona = !is.na(twpx$tag)

        twpx = twpx[nona,]

        twpx = as.list(twpx)
        
        A1T = rangedatetime(twpx)
        s1 = secdifL(A1T$min,  twpx)

        nh$pickfile =  INITpickfile(stas=nh$sta, src=NULL, WPX=twpx)

      }
    
    dev.set(dev.next() )
    
    eqsol = NLSlocate(nh, v=nh$vel,  PLOT=TRUE )
    dev.set( g$MAINdev)

    g$zloc = list(x=NULL, y=NULL)
    g$action="donothing"
    invisible(list(global.vars=g))
    
  }
