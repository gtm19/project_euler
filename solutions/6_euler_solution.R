# PROJECT EULER EXERCISE 6:
#
# The sum of the squares of the first ten natural numbers is,
# 1^2 + 2^2 + ... + 10^2 = 385
# 
# The square of the sum of the first ten natural numbers is,
# (1 + 2 + ... + 10)^2 = 552 = 3025
#
# Hence the difference between the sum of the squares of the first ten natural
# numbers and the square of the sum is 3025 − 385 = 2640.
# 
# Find the difference between the sum of the squares of the first one hundred
# natural numbers and the square of the sum.

euler_six <- function(n = 100) {
  
  product_matrix <- outer(1:n, 1:n)
  
  sum(product_matrix) - sum(diag(product_matrix))
  
}

euler_six()