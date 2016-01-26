### Approximate Pi
set.seed(123454321)

# Pi approximation function
piApprox <- function(n) {
  u <- runif(n, -1, 1)
  v <- runif(n, -1, 1)
  w <- u ^ 2 + v ^ 2 <= 1
  x <- 4 * sum(w)
  c(x = x, n = n)
}

# Pi approximation iterations 
out <- numeric(2)
for(i in 1:10000){
  out <- out + piApprox(100000)
  piEst <- out['x'] / out['n']
  cat(i, piEst, piEst - pi, fill = T)
  cat(piEst, fill = T, append = i > 1, file = 'pi.txt')
}

#plot(scan('pi.txt'))