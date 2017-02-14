a <- matrix(seq(1, 8), nrow = 4, ncol = 2)
b <- c(4, 5, 6, 7)
c <- a*b
plot(c,type="l")
q <- which(c == max(c), arr.ind = TRUE)
