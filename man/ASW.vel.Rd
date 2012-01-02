\name{ASW.vel}
\alias{ASW.vel}
\alias{LITHOS.vel}
\alias{StHelens4.vel}
\alias{fuj1.vel}
\alias{jap1.vel}
\alias{montserat.vel}
\alias{wu_coso.vel}
\title{1D Velocity Ecuador}
\description{1D Velocity Ecuador
}
\usage{
data(ASW.vel)
}
\format{a list of velocities for hypocenter relocation
}
\source{
  Mario Ruiz
}

\examples{

data(ASW.vel)
data(wu_coso.vel)
data(fuj1.vel)
data(LITHOS.vel)

Comp1Dvels(c("ASW.vel","wu_coso.vel",  "fuj1.vel" , "LITHOS.vel"  ))


}

\keyword{datasets}
