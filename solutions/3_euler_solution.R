# PROJECT EULER EXERCISE 3:
#
# The prime factors of 13195 are 5, 7, 13 and 29.
# 
# What is the largest prime factor of the number 600851475143?

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
  
  message("The unique prime factorisation of")
  message(paste0(format(n, big.mark = ","), " is:"))
  message(paste(sort(factors), collapse = " x "))
  message("So the biggest prime factor is:")
  print(max(factors))
}

euler_three()
