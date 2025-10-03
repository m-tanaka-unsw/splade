##########################################################################################
#
# Unit Testing: distance_between
# By: Monique Vith
# Supervisor: Mark Tanaka
#
##########################################################################################

##########################################################################################
#
#                                 TESTING ERROR CASES
#
##########################################################################################

# Error case: check v1 and v2 are numeric and positive
test_that("v1 and v2 are invalid", {
  # v1 is not a numeric
  expect_error(distance_between("hi", 2, egfoliox_initial))

  # v2 is not a numeric
  expect_error(distance_between(2, "hi", egfoliox_initial))

  # both v1 and v2 are not numeric
  expect_error(distance_between("hi", "hi", egfoliox_initial))

  # v1 is negative
  expect_error(distance_between(-1, 0, egfoliox_initial))
})

# Error: foliox is not the correct class
test_that("Test should throw an error if foliox object given is not of class foliox", {
  expect_error(distance_between(1, 2, 3))
})

# check v1 and v2 are not in foliox
test_that("v1 and v2 are in the given foliox", {
  # v1 not in foliox
  expect_error(distance_between(35, 2, egfoliox_initial))

  # v2 not in foliox
  expect_error(distance_between(2, 35, egfoliox_initial))

  # v1 and v2 not in foliox
  expect_error(distance_between(35, 36, egfoliox_initial))
})

##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# check return type is a numeric
test_that("return type for distance_between is numeric", {
  results <- distance_between(1, 3, egfoliox_initial)
  expect_type(results, "double")
})

# check v2 less than v1
test_that("v2 is less than v1", {
  results <- distance_between(5, 1, egfoliox_initial)
  vertex_2 <- egfoliox_initial$weight[2]
  vertex_3 <- egfoliox_initial$weight[3]
  vertex_4 <- egfoliox_initial$weight[4]
  vertex_5 <- egfoliox_initial$weight[5]
  total_distance <- vertex_2 + vertex_3 + vertex_4 + vertex_5
  expect_equal(results, total_distance)
})

# check v2 greater than v1
test_that("v2 is greater than v1", {
  results <- distance_between(1, 5, egfoliox_initial)
  vertex_2 <- egfoliox_initial$weight[2]
  vertex_3 <- egfoliox_initial$weight[3]
  vertex_4 <- egfoliox_initial$weight[4]
  vertex_5 <- egfoliox_initial$weight[5]
  total_distance <- vertex_2 + vertex_3 + vertex_4 + vertex_5
  expect_equal(results, total_distance)
})

# check v1 equals v2
test_that("v1 equals v2", {
  results <- distance_between(1, 1, egfoliox_initial)
  expect_equal(results, 0)
})

# check if v1 is head (0)
test_that("v1 is the head", {
  results <- distance_between(0, 2, egfoliox_initial)
  vertex_1 <- egfoliox_initial$weight[1]
  vertex_2 <- egfoliox_initial$weight[2]
  total_distance <- vertex_1 + vertex_2
  expect_equal(results, total_distance)
})

# check if both v1 and v2 are 0 
test_that("v1 and v2 are both zero", {
  results <- distance_between(0, 0, egfoliox_initial)
  expect_equal(results, 0)
})


