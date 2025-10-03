##########################################################################################
#
# Unit Testing: avg_binary_fraction
# By: Monique Vith
# Supervisor: Mark Tanaka
#
##########################################################################################

##########################################################################################
#
#                                 TESTING ERROR CASES
#
##########################################################################################

# error case: Invalid string inputs given 
test_that("Invalid string inputs given", {
  
  # invalid string 1
  expect_error(avg_binary_fraction("122", "001"))
  
  # invalid string 2
  expect_error(avg_binary_fraction("001", "897"))
  
  # invalid string 1 and string 2
  expect_error(avg_binary_fraction("435", "897"))
  
  # decimal point given
  expect_error(avg_binary_fraction("4.0", "897"))
  
})

# error case: Invalid string type
test_that("Invalid string type", {
  
  # invalid string 1
  expect_error(avg_binary_fraction(101, 101))
  
  
})


##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################


# passing case: binary strings of different lengths 
test_that("Given binary strings of different lengths", {
  s1 <- "1000"
  s2 <- "1"
  output <- avg_binary_fraction(s1, s2)
  expected <- "10000"
  
  expect_equal(output, expected)
})

# passing case: binary strings of same lengths 
test_that("Given binary strings of same lengths", {
  s1 <- "0100"
  s2 <- "1001"
  output <- avg_binary_fraction(s1, s2)
  expected <- "01101"
  
  expect_equal(output, expected)
})

# passing case: binary strings with multiple carries
test_that("Given binary strings with multiple carries", {
  s1 <- "1111"
  s2 <- "1111"
  output <- avg_binary_fraction(s1, s2)
  expected <- "10000"
  
  expect_equal(output, expected)
})

# passing case: binary strings with leading zeros
test_that("Given binary strings with leading zeros", {
  s1 <- "0011"
  s2 <- "1111"
  output <- avg_binary_fraction(s1, s2)
  expected <- "10000"
  
  expect_equal(output, expected)
})


# passing case: binary strings with no carries
test_that("Given binary strings with no carries", {
  s1 <- "1010"
  s2 <- "0101"
  output <- avg_binary_fraction(s1, s2)
  expected <- "01111"
  
  expect_equal(output, expected)
})


