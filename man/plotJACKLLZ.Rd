\name{plotJACKLLZ}
\alias{plotJACKLLZ}
\title{BoxPlot Jackknife of station locations
}
\description{BoxPlot Jackknife of station locations
}
\usage{
plotJACKLLZ(hjack, sta, proj = NULL,  PLOT=0, PS=FALSE,
    fbase="jack", width =c(10, 5) , height = c(8, 8))
}
\arguments{
  \item{hjack}{Output of hijack
}
  \item{sta}{station location list
}
  \item{proj}{projection list
}
 \item{PLOT}{plotting flag, 0,1,2. If PLOT=0 plot both boxplots and map,
if plot =1 plot only boxplot, if plot=2 plot only map. Default=0
}
 \item{PS}{postscript output device, default=FALSE
 }
  \item{fbase}{basename for output png files }
 \item{ width}{2-vector of width of plots output (inches) }
 \item{ height}{2-vector of height of plots output (inches) }

}
\details{
  takes the output of the HiJack function and extracts the pseudovalues
  and influence information for boxplots.
}
\value{
  Graphical side effects and
    \item{X}{influence of lon
}
  \item{Y}{influence of lat
}
  \item{Z}{influence of depth
  }
  \item{files}{character vector of the names of the files output
  }

  
}
\references{
  Iversen, E. S., and J. M. Lees (1996), A statistical technique for
  validating velocity models,
  Bull. Seismol. Soc. Am. 86(6), 1853-1862.

}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{HiJACK, BLACKJACK,imageINFLUENCE
}
\examples{

\dontrun{

COSOjack = HiJACK(lps, sta)

plotJACKLLZ(COSOjack, sta, proj)



}






}
\keyword{misc}
