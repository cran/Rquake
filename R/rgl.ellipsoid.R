rgl.ellipsoid <- function (x=0, y=0, z=0, a = 1, b=1, c=1, phi=0, theta=0, psi=0,
                           subdivide = 3, smooth = TRUE, ...)
{

    ###  code borrowed from Package: cda by Baptiste Auguie
  
  sphere <- rgl::subdivision3d(rgl::cube3d(...), subdivide)
  class(sphere) <- c("mesh3d","shape3d")
  
  norm <- sqrt(sphere$vb[1, ]^2 + 
                 sphere$vb[2, ]^2 + 
                 sphere$vb[3, ]^2 )
  
  for (i in 1:3) sphere$vb[i, ] <- sphere$vb[i, ]/norm
  sphere$vb[4, ] <- 1
  sphere$normals <- sphere$vb
  result <- rgl::scale3d(sphere, a,b,c)
  rotM <- euler_passive(phi,theta,psi)
  result <- rgl::rotate3d(result,matrix=rotM)
  result <- rgl::translate3d(result, x,y,z)
  invisible(result)
}
