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

euler_three <- function(n = 600851475143) {
  
  primenums <- primes(sqrt(n))
  factors <- primenums[which(n %% primenums == 0)]
  
  while(n / prod(factors) > 1) {
    newnum <- n/prod(factors)
    primenums <- primes(sqrt(newnum))
    
    newfactors <- primenums[which(newnum %% primenums == 0)]
    
    if(length(newfactors) == 0) {
      factors <- c(factors, newnum)
    } else factors <- c(factors, newfactors)
  }
  
  max(factors)
}

euler_three()
