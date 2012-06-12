\name{rglGEOmapXY}
\alias{rglGEOmapXY}
\title{High Level plot of GEO map

}
\description{High Level plot of GEO map using rgl graphics

}
\usage{
rglGEOmapXY(MAP, LIM = c(-180, -90, 180, 90), PROJ = list(),
PMAT = NULL, add = TRUE, SEL = NULL, GRID = NULL, GRIDcol = 1,
MAPcol = NULL, MAPstyle = NULL, border = NA, cenlon = 0,
shiftlon = 0, linelty = 1, linelwd = 1, ptpch = ".",
ptcex = 1, NUMB = FALSE, ...)
}
\arguments{
 \item{MAP}{Map Structure}
  \item{LIM}{Lat-Lon limits}
  \item{PROJ}{Projection list }
  \item{PMAT}{Perspective matrix conversion}
  \item{add}{logical, TRUE= add to existing plot}
  \item{SEL}{Index vector of strokes to be used in plotting,default=NULL(use all that pass other tests) }
  \item{GRID}{logical, TRUE=add grid lines}
  \item{GRIDcol}{color for grid lines}
  \item{MAPcol}{override color for maps}
  \item{MAPstyle}{override plotting style for maps}
  \item{border}{color, add border to polygons, NA=no border}
  \item{cenlon}{center longitude of plot}
  \item{shiftlon}{degrees, rotate longitude}
  \item{linelty}{Line type}
  \item{linelwd}{line width}
  \item{ptpch}{plotting character for strokes (style=1) that are plotted
  as points}
  \item{ptcex}{character expansion factor for style=1 strokes }


  \item{NUMB}{logical, number the strokes on the map}

  \item{\dots}{graphical parameters
}
}
\details{The function duplicates the
  plotGEOmapXY code but instead returns
  coordinates suitable for rgl spinning in 3D.
}
\value{Graphical Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{This is used in conjunction with other 3D plotting.
}


\seealso{ROTcovQUAKE
}
\examples{


\dontrun{
ROTcovQUAKE(cosoqk[5:10], proj , domap=FALSE)
 rglGEOmapXY(cosomap,   PROJ =proj, MAPstyle=2, add=TRUE )


}






}
\keyword{misc}
