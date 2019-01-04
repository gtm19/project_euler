euler_one <- function(n = 1000) {
  
  sum(which(1:(n-1) %% 3 == 0 | 1:(n-1) %% 5 == 0))
  
}

euler_one()