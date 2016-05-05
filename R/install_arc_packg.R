# Download package tarball from CRAN archive

url <- "http://cran.r-project.org/src/contrib/Archive/WaveletCo/WaveletCo_1.0.tar.gz"
pkgFile <- "WaveletCo_1.0.tar.gz"
download.file(url = url, destfile = pkgFile)

# Install dependencies

#install.packages(c("ada", "ipred", "evd"))

# Install package
install.packages(pkgs=pkgFile, type="source", repos=NULL)

# Delete package tarball
unlink(pkgFile)
