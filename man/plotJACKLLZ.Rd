\name{plotJACKLLZ}
\alias{plotJACKLLZ}
\title{BoxPlot Jackknife of station locations
}
\description{BoxPlot Jackknife of station locations
}
\usage{
plotJACKLLZ(hjack, sta, proj = NULL,  PLOT=0, PS=FALSE)
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
