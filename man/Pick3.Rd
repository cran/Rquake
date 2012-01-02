\name{Pick3}
\alias{Pick3}

\title{PICK Buttons for swig
}
\description{Picking functions for swig
}
\usage{
Pick3(nh, g)

}
\arguments{
  \item{nh}{waveform list for RSEIS
}
  \item{g}{plotting parameter list for interactive program
}


}
\details{

  Buttons can be defined on the fly.
  \describe{
    \item{Pick3}{Multiple picks on a panel}
   

}
  
}
\value{The return value depends on the nature of the function
  as it is returned to the main code swig.
Choices for returning to swig are: break, replot, revert, replace,
donothing, exit.


  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig, PickWin
}
\examples{

\dontrun{

MYFUNC<-function(nh, g)
  {
    print("pressed MYFUNC")
    d = data.frame(list(stations=nh$STNS, components=nh$COMPS))
print(d)        
    g$action = "replot"
    invisible(list(global.vars=g))	
  }

STDLAB=c("DONE", "QUIT", "SELBUT" , "MYFUNC" )
data(GH)
JJ = swig(GH, sel=1:10, STDLAB=STDLAB)

}


}
\keyword{misc}
