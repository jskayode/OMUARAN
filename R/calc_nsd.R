#########################################################################
# This function calculates nsd                                          
# Author John Stephen Kayode                                            
#                                                                       
# List of input:                                                                 
# 
# 1. winsize = (window size for std calculation. Must be odd & >= 3)
# 2. nsd = (struc_index,winsize,z)                                  
# 3. dz = (vertical_derivative)                                      
# 4. zt = (taper2spline)
# 5. z = in matrix form
# 6. s = structural index, 0.5 to 3 by 0.5
########################################################################

#install.packages('pracma')
#install.packages('numDeriv')

calc_nsd <- function(s, winsize, z) {
  # s = structural index, a value of 0.5 to 3 by increments of 0.5
  # winsize = window size must be be square window 3, 5, 7, etc
  # z = data ???
  
  # This function calculates nsd (standard deviation of the 'derivates')
  # This function depends on the convolve and rotate functions
  library(pracma) # To use the nextpow2 and gradient functions
  source('R/convolve.R')
  source('R/rotate.R')
  source('R/vertical_gradient.R')
  source('R/taper2spline.R')
  
  
  
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

  # Convolve the 3x3 data if it has a window size of 3
  z_conv = convolve(z, se)

  ## Compute gradients
  
  # Determine the dimensions of the data (matrix)
  n_row <- nrow(z)
  n_col <- ncol(z)
  dimensions <- c(n_row,n_col)
  # Determine the max dimension of the data
  nmax <- max(dimensions)
  # calculate npts = next points???
  npts = 2 * nextpow2(nmax)

  # Calculate the gradient
  z_gradient = gradient(z_conv) # A list of two matrices ???
  
  
  # Calculate vertical gradient using custom function
  dz = vertical_gradient(z, npts, nc, nr, 1)

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


 













