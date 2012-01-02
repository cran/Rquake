\name{GPIX}
\alias{GPIX}

\title{PICK Buttons for swig
}
\description{defining functions for swig
}
\usage{
GPIX(nh, g)

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
    \item{GPIX}{Multiple picks on a panel}
 

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


\seealso{swig, XTR
}
\examples{

\dontrun{


STDLAB=c("DONE", "QUIT", "SELBUT" , "GPIX" )
data(GH)
JJ = swig(GH, sel=1:10, STDLAB=STDLAB)

}


}
\keyword{misc}
