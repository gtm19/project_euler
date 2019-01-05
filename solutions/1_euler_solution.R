# PROJECT EULER EXERCISE 1:
#
# If we list all the natural numbers below 10 that are multiples of 3 or 5,
# we get 3, 5, 6 and 9. 
#
# The sum of these multiples is 23.
# 
# Find the sum of all the multiples of 3 or 5 below 1000.


euler_one <- function(n = 1000) {
  
  sum(which(1:(n-1) %% 3 == 0 | 1:(n-1) %% 5 == 0))
  
}

euler_one()