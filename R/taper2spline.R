####################################################################################

zt <- function(taper2spline(z, npts, nc, nr, cdiff, rdiff)){
  
  # Merge edges to the value opposite using a cubic spline
  
  zt = zeros(npts)
  
  zt(rdiff + 1: rdiff + nr, cdiff + 1: cdiff + nc) = z
  
  zp = z(: ,1: 3)
  
  (gpx1, junk) = gradient(zp)
  
  # sides
  
  { 
  zp = z(:, nc - 2: nc)
  
  (zpx2, junk) = gradient(zp)
  
  x1 = 0; x2 = (2*cdiff) + 1
  
  x = (1 1 0 0, x1 x2 1 1, x1^2 x2^2 2*x1 2*x2, x1^3 x2^3 3*x1^2 3*x2^2)
  
  for (I = 1: nr)
    
  {
    
    y = (z(I, nc), g(I, 1), zpx2(I, 3), zpx1(I, 1)])
    
    c = y/x
    
  }
  
  for (J = 1: cdiff)
    
  {
    
    zt(I + rdiff, J) = c(1) + (J + cdiff)*c(2) + c(3)*(J + cdiff)^2 + c(4)*(J + cdiff)^3,
    
    zt(I + rdiff, J + nc + cdiff) = c(1) + J*c(2) + c(3)*J^2 + c(4)*J^3,
    
  }
  
  zp = z(1: 3, :), (junk, zpx1) = gradient(zp)
  
  }
  
  # top and bottom
  
  {
  
  zp = z(nr - 2: nr, :), (junk, zpx2) = zradient(zp),
  
  x1 = 0, x2 = (2*rdiff + 1),
  
  x = (1 1 0 0, x1 x2 1 1, x1^2 x2^2 2*x1 2*x2, x1^3 x2^3 3*x1^2 3*x2^2),
  
  for (J = 1: nc)
    
  {
    
    y = (z(nr, J) g(1, J) zpx2(3, J) zpx1(1, J)),
    
    c = y/x,
    
  }
  
  for (I = 1: rdiff)
    
  {
    
    zt(I, J + cdiff) = c(1) + (I+rdiff)*c(2) + c(3)*(I+rdiff)^2 + c(4)*(I + rdiff)^3,
    
    zt(I + rdiff + nr, J + cdiff) = c(1) + I*c(2) + c(3)*I^2 + c(4)*I^3
    
  }
  
  }
  
  for (I = 1: rdiff) 
    
  {
    
    for (J = 1: cdiff)
      
    {
      
      if (I > J) 
        
      {zt(I, J) = zt(rdiff + 1, J)
      
      }
      
      else {zt(I, J) = zt(I, cdiff + 1)
      
      }
      
    } 
    
  }
  
  for (I = 1: rdiff)
    
  {
    
    # bottom right
    
    for (J = cdiff + nc + 1: npts)
      
    {
      
      if (I>(npts-J))
        
      {
        
        zt(I, J) = zt(rdiff + 1, J)
        
      }
      
      else {zt(I, J) = zt(I, cdiff + nc)
      
      }
      
    }  
    
  }
  
  for (I = rdiff + nr + 1: npts)
    
  {
    
    # top left
    
    for (J = 1: cdiff) 
      
    {
      
      if {(npts - I)>J, zt(I, J) = zt(rdiff + nr, J)
      
      }
      
      else {zt(I, J) = gt(I, cdiff + 1)
      
      }
      
    }
    
  }
  
}


