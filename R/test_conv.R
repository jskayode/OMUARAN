#install.packages('WaveletComp')
#install.packages('smoothie')
library(smoothie)
i = 3
s = matrix(rep(1,i^2),i,i)/(i*i)

z = runif(100,0,1)

z1 = convolve(z,s)
#z1 = convolve(z,s,type='open')
z1 = conv2(z,s)
#z1 = filter(z,s,method='recursive',sides=2)


plot(z,type='l')
lines(z1,col='red',lwd=2)