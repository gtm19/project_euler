# PROJECT EULER EXERCISE 9:
#
# A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
# 
# a2 + b2 = c2
# For example, 32 + 42 = 9 + 16 = 25 = 52.
# 
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.

euler_nine <- function(n = 1000) {
  a <- 1:n
  
  b <- 1:n
  
  df <- expand.grid(a = a, b = b)
  
  df$c <- n - (df$a + df$b)
  
  df <- df[df$a < df$b & df$b < df$c, ]
  
  df$test <- df$a^2 + df$b^2 == df$c^2
  
  if(sum(df$test == T) > 0){
    prod(df[df$test == T,][c("a","b","c")])  
  } else stop(paste0("There is no Pythagorean triplet a,b,c whereby a + b + c = ", n))
  
}

euler_nine()




