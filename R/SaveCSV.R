SaveCSV <-
function(nh, g )
  {

    destdir = "."
    
        twpx= g$WPX
        
        nona = which( is.na(twpx$tag) )
        
        if(length(nona)>0)
          {
            twpx = deleteWPX(twpx, nona)
          }
         if(length(twpx$tag)<1 )
           {
             
             g$action="donothing"
             invisible(list(global.vars=g))
           }
        
        RDATES = rangedatetime(twpx)
        
        fout1 = PCfiledatetime(RDATES$min, 0)
        
        fout2 = paste(fout1,"csv", sep="." )
        
        fout3 = paste(destdir, fout2, sep="/")
        
        write.csv(twpx, file=fout3)
        
    
    g$zloc = list(x=NULL, y=NULL)
    g$action="donothing"
    invisible(list(global.vars=g))
  }
