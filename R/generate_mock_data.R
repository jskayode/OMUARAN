# create randomized data for testing
z1 <- runif(500, min = -650, max = 550)
z2 <- runif(500, min = -650, max = 550)
z3 <- runif(500, min = -650, max = 550)

z <- data.frame(z1, z2, z3)
z <- as.matrix(z)
