SavePF <-
function(nh, g )
  {
    
    
    if(is.null(nh$pickfile))
      {
        print("SavePF: saving raw pix")
        if(is.null(g$WPX) )invisible(list(global.vars=g))
        

        twpx= g$WPX
        
        nona = which( is.na(twpx$tag) )
        
        if(length(nona)>0)
          {
            twpx = deleteWPX(twpx, nona)
          }
        
        
        if(length(twpx$tag)<1 )invisible(list(global.vars=g))
        
        saveWPX(twpx, destdir="." )
        
      }
    else
      {

        if(is.null( nh$sta))
          {
            print("no station file")
            invisible(list(global.vars=g))
          }
        
    
        if( all(is.na( nh$sta)) )
          {
            print("no station file")
            invisible(list(global.vars=g))
          }
    
        
        
        
        PFoutput(nh$pickfile, nh$sta,  sol=NULL, format=c(1,2) )
      }
    
    
    
    g$zloc = list(x=NULL, y=NULL)
    g$action="donothing"
    invisible(list(global.vars=g))
  }
