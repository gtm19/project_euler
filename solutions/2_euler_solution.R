euler_two <- function(n = 4e6) {
  
  fib <- c(1,2)
  
  while (max(fib) < n) {
    fib <- c(fib, sum(rev(fib)[1:2]))
  }
  
  sum(fib[which(fib %% 2 == 0)])
  
}

euler_two()