euler_passive<-function(phi,theta,psi)
    {
        ###  code borrowed from Package: cda by Baptiste Auguie
  
     Rot<-matrix(0, ncol=3,nrow=3);
     
     cosphi = cos(phi); cospsi = cos(psi); costheta = cos(theta);
     sinphi = sin(phi); sinpsi = sin(psi); sintheta = sin(theta);
    Rot[1,1] = cosphi*costheta*cospsi - sinphi*sinpsi;
    Rot[1,2] = sinphi*costheta*cospsi + cosphi*sinpsi;
    Rot[1,3] = -sintheta*cospsi;

    Rot[2,1] = -cosphi*costheta*sinpsi - sinphi*cospsi;
    Rot[2,2] = -sinphi*costheta*sinpsi + cosphi*cospsi;
    Rot[2,3] = sintheta*sinpsi;

    Rot[3,1] = cosphi*sintheta;
    Rot[3,2] = sinphi*sintheta;
    Rot[3,3] = costheta;

     return(Rot)
 }
