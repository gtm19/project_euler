# PROJECT EULER EXERCISE 7:
#
# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
# we can see that the 6th prime is 13.
#
# What is the 10 001st prime number?

primes <- function(n) {
  
  n <- floor(n)
  
  primes <- c(FALSE, rep(TRUE, n-1))
  
  max_p <- floor(sqrt(n))
  
  p <- 2L
  
  while(p <= max_p) {
    primes[seq.int(2L*p, n, p)] <- FALSE
    
    p <- min(subset(which(primes), which(primes) > p))
  }
  
  which(primes)
}

euler_seven <- function(n = 10001) {
  # Upper limit approximation from https://bit.ly/2Qpov88
  upper_lim <- floor(n*(log(n) +  log(log(n))))
  
  primes(upper_lim)[n]
  
}

euler_seven()