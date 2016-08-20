\name{Qrangedatetime}
\alias{Qrangedatetime}
\title{Range of Date Time}
\description{
  Return the range of dates and times for any list with a date/time list
}
\usage{
Qrangedatetime(D)
}
\arguments{
  \item{D}{info list from RSEIS seismic data list}
}

\value{
  \item{min}{date time list}
  \item{max}{date time list}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

\dontrun{
library(RSEIS)
data(GH)

Qrangedatetime(GH$info)

}


}
\keyword{misc}
