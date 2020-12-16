checkLOCATEinput<-function(Ldat, EQ, vel=NULL)
    {

      if( any( !is.numeric(Ldat$x) ) |
         any( !is.numeric(Ldat$y) ) |
         any( !is.numeric(Ldat$z) ) )
          {
              cat('STOP:\n')
              cat(' Ldat has bad data.\n')
              return(FALSE)
          }

      if( any( !is.numeric(EQ$x) ) |
         any( !is.numeric(EQ$y) ) |
         any( !is.numeric(EQ$z) ) |
         any( !is.numeric(EQ$t) )
         )
          {
              cat('STOP:\n')
              cat(' EQ has bad/missing data.\n')
              return(FALSE)
          }


      ##### passes inspection

      return(TRUE)

    }
