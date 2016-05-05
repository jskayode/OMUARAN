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

