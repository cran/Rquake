rgl.ellipsoids <- function(positions, sizes, angles, col = "red", ...)
{
  
  N <- NCOL(positions)
  colors <- rep(col, length.out=N)
  ll <- lapply(seq(1,N), function(ii)
    rgl.ellipsoid(positions[1,ii],positions[2,ii],positions[3,ii],
                  sizes[1,ii],sizes[2,ii],sizes[3,ii],
                  angles[1,ii],angles[2,ii],angles[3,ii],
                  col = colors[ii], ...))
  
  rgl::shapelist3d(ll)
  
}
