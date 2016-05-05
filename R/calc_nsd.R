#########################################################################
# This function calculates nsd                                          
# Author John Stephen Kayode                                            
#                                                                       
# List of input:                                                                 
# 1. COMPLETE.DATA = (matrix of data for processing)                
# 2. winsize = (window size for std calculation. Must be odd & >= 3)
# 3. nsd = (struc_index,winsize,z)                                  
# 4. dz = (vertical_derivative)                                      
# 5. zt = (taper2spline)
# 6. z = in a matrix form
# 7. s = structural index, 0.5 to 3 by 0.5
########################################################################

calc_nsd <- function(s, winsize, z) {
  # s = structural index, a value of 0.5 to 3 by increments of 0.5
  # winsize = window size must be be square window 3, 5, 7, etc
  # z = data ???
  
  # This function calculates nsd (standard deviation of the 'derivates')
  # This function depends on the convolve and rotate functions
  source('R/convolve.R')
  source('R/rotate.R')
  
  # Check for sensible values of window size for st dev calculation
  # Window size has to be a square matrix such as 3x3, 5x5, 7x7, etc.
  # The larger the window size the lower the resolution
  
  # Just in case a negative value was provided
  winsize = abs(winsize)
  
  # Use minimum value of 3 if a < 3 value is provided
  if (winsize < 3) {
    winsize = 3
  }
  # To maintain a minimum value of 3 (again)
  winsize = 1 + 2 * floor(winsize/2)
   
  
  # Smooth the data first
  # This is done by creating a square matrix of 1 and dividing by 
  # the square of the structural index (s)
  # se = 1/s^2
  
  if (s > 0) {                        
    se = matrix(rep(1,winsize^2),winsize,winsize)/(s * s)
  }

  z = convolve(z, se)



  # Compute gradients
  
  (nr, nc) = size(z) 
  nmax = max([nr, nc])
  npts = 2 * nextpow2(nmax)

   (dx, dy) = gradient(z)
  dz = vertical(z, npts, nc, nr, 1)

  # Compute windowed std of gradients

   w2 = floor(wsize/2)

      dxv = zeros(nr,nc), dyv = zeros(nr,nc), dzv = zeros(nr,nc)

   for (I = w2 + 1: nr- w2)
   {
      
   if I/20 == floor(I/20)

    {
     
      disp(I)
     
   }

   for (J = w2 + 1; nc - w2)
     
     {
     
     wx = dx(I - w2: I + w2, J - w2: J + w2)
     
     dxv(I, J) = std(wx(:))
     
     wy = dy(I - w2: I + w2, J - w2: J + w2); dyv(I, J) = std(wy(:))
     
     wz = dz(I - w2: I + w2, J - w2: J + w2); dzv(I, J) = std(wz(:))
     
   }
   
   }

  # NSD

   nsd = dzv./(dxv + dyv + dzv)

   nsd = nsd(w2 + 1: nr - w2, w2 + 1: nc - w2)

 # Display results

  f = figure(1), (clf)

  set(if, "Name", "COMPLETE.DATA")

  set(if, "NumberTitle", "off")

  ax(1) = subplot(1, 2, 1); imagesc(Z)
       axis equal, axis tight, axis off, axis xy, title(COMPLETE.DATA)
 
  ax(2) = subplot(1, 2, 2), imagesc(nsd)
       axis equal, axis tight, axis off, axis xy, title(Normalised SD)

  linkaxes(ax, xy)

  colormap(hsv(256))
}


 ###################################################################################
 # calc_dz <- function(vertica_derivative(COMPLETE.DATA, npts, nc, nr, xint)) 

 # Compute vertical derivative

  cdiff = floor((npts - nc)/2)

   rdiff = floor((npts - nr)/2)

       data1 = taper2spline(COMPLETE.DATA, npts, nc, nr, cdiff, rdiff)

      f = fft2(data1) 

      fz = f

     wn = 2.0*pi/(xint*(npts - 1))

      f = fftshift(f)

      cx = npts/2+1 

      cy = cx;
  {
  
  for (I = 1: npts)
  {
      freqx = (I - cx)*wn
  }
  
  }
  
  for (J = 1: npts)
  
    {
  
     freqy = (J - cy)*wn
  
     freq = sqrt(freqx*freqx + freqy*freqy)
  
     fz(I,J) = f(I,J)*freq
     
  }

    zg = fftshift(fz)

   fzinv = ifft2(fz)

    dz = real(fzinv(1 + rdiff: nr + rdiff, 1 + cdiff: nc + cdiff))


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















