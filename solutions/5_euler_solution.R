# PROJECT EULER EXERCISE 5:
#
# 2520 is the smallest number that can be divided by each of the numbers
# from 1 to 10 without any remainder.
# 
# What is the smallest positive number that is evenly divisible by all 
# of the numbers from 1 to 20?

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

euler_five <- function(n = 20) {
  
  names <- primes(n)
  
  df <- data.frame(matrix(nrow = 0, ncol = length(names)))
  
  names(df) <- names
  
  for(i in 1:n){
    
    primenums <- primes(sqrt(i))
    factors <- primenums[which(i %% primenums == 0)]
    
    while(i / prod(factors) > 1) {
      newnum <- i/prod(factors)
      primenums <- primes(sqrt(newnum))
      
      newfactors <- primenums[which(newnum %% primenums == 0)]
      
      if(length(newfactors) == 0) {
        factors <- c(factors, newnum)
      } else factors <- c(factors, newfactors)
    }
    
    
    df[i,] <- sapply(names, function(x) sum(sort(factors) == x))
    
  }
  
  prod(mapply(function(x,y) x^y, as.numeric(names(df)), sapply(df, max)))
  
}

euler_five()
