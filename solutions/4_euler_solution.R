# PROJECT EULER EXERCISE 4:
#
# A palindromic number reads the same both ways. 
# The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
#
# Find the largest palindrome made from the product of two 3-digit numbers.

palindrome <- function(n) {
  order <- nchar(n):1
  
  digits <-
    sapply(order, function(order) {
      ((n %% 10^order) - (n %% 10^(order - 1)))/(10^(order - 1))
    })
  
  all(rev(digits) == digits)
}

euler_four <- function(nums = as.vector(sapply(100:999, function(x) x*100:999))) {
  
  nums <- sort(nums, decreasing = T)
  
  index <- 1
  
  repeat {
    if(palindrome(nums[index])) break
    index <- index + 1
  }
  
  nums[index]

}

euler_four()
