\name{XYerror.bars}
\alias{XYerror.bars}
\title{Error Bars in X and Y
}
\description{Error Bars in X and Y
}
\usage{
XYerror.bars(x, y, xlo = 0, xhi = 0, ylo = 0, yhi = 0, pch = 1, col = 1, barw = 0.1, add = FALSE, ...)
}
\arguments{
  \item{x}{X-values

}
  \item{y}{Y-values

}
  \item{xlo}{X Lower limit of error bars

}
  \item{xhi}{X Upper limit of error bars
}
  \item{ylo}{Y Lower limit of error bars
}
  \item{yhi}{Y  Upper limit of error bars
}
  \item{pch}{plotting character

}
  \item{col}{color

}
  \item{barw}{width of the bar (inches)
}
  \item{add}{logical, add=FALSE starts a new plot
}
  \item{\dots}{other plotting parameters
}
}

\value{graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{


set.seed(0)
zup = rnorm(10)

x = 1:10
y = 2*x+5+zup

ydown = rnorm(10)
ydown = ydown-min(ydown)+.2

yup = rnorm(10)
yup = yup-min(yup)+.2



zup = rnorm(10)
xup = zup-min(zup)+.5
xdown = rnorm(10)
xdown = xdown-min(xdown)+.2


####  example with different  error on either side:
XYerror.bars(x, y, y-ydown, y+yup, x-xdown, x+xup,  pch = 1, col = 'brown' , barw = 0.1, add
= FALSE)



}
\keyword{misc}
