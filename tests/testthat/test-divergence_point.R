##########################################################################################
#
# Unit Testing: divergence_point
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
  expect_error(divergence_point("hi", 2, egfoliox_initial))

  # v2 is not a numeric
  expect_error(divergence_point(2, "hi", egfoliox_initial))

  # both v1 and v2 are not numeric
  expect_error(divergence_point("hi", "hi", egfoliox_initial))

  # v1 is negative
  expect_error(divergence_point(-1, 0, egfoliox_initial))

  # v2 is negative
  expect_error(divergence_point(0, -1, egfoliox_initial))

  # v1 and v2 are negative
  expect_error(divergence_point(-1, -1, egfoliox_initial))
})

# check foliox argument is a valid foliox class
test_that("invalid foliox given", {
  expect_error(divergence_point(1, 2, 3))
})

# check v1 and v2 are not in foliox
test_that("v1 and v2 are not in the given foliox", {
  # v1 not in foliox
  expect_error(divergence_point(35, 2, egfoliox_initial))

  # v2 not in foliox
  expect_error(divergence_point(2, 35, egfoliox_initial))

  # v1 and v2 not in foliox
  expect_error(divergence_point(35, 36, egfoliox_initial))
})


##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# check return type is a numeric
test_that("return type for distance_between is numeric", {
  results <- divergence_point(1, 3, egfoliox_initial)
  expect_type(results, "double")
})

# check v1 and v2 are equal
test_that("v1 and v2 are equal", {
  result <- divergence_point(1, 1, egfoliox_initial)
  expect_equal(result, 1)

  result <- divergence_point(12, 12, egfoliox_initial)
  expect_equal(result, 12)
})

# check v1 and v2 are equal to 0
test_that("v1 and v2 are equal to 0", {
  result <- divergence_point(0, 0, egfoliox_initial)
  expect_equal(result, 0)
})

# v1 and v2 are on the same path
test_that("v1 and v2 are on same path", {
  result <- divergence_point(10, 16, egfoliox_initial)
  expect_equal(result, 10)
})

# v1 and v2 are on different stems
test_that("v1 and v2 are on different stems", {
  result <- divergence_point(5, 19, egfoliox_initial)
  expect_equal(result, 2)
})

# Add a leaf to foliox and check divergence point is still valid when on different paths
test_that("v1 and v2 are on different stems and a bud is within the path", {
  tree <- add_a_leaf_to_foliox(10, egfoliox_initial, "test",1,0.0606858207257033)
  result <- divergence_point(11, 19, tree)
  expect_equal(result, 9)
})

# Add a leaf to foliox and check divergence point is still valid when on same paths
test_that("v1 and v2 are on different stems and a bud is within the path", {
  tree <- add_a_leaf_to_foliox(10, egfoliox_initial, "test",1,0.0606858207257033)
  result <- divergence_point(9, 34, tree)
  expect_equal(result, 9)
})

# no common ancestor
test_that("v1 and v2 have no common ancestor", {
  result <- divergence_point(33, 31, egfoliox_initial)
  expect_equal(result, 0)
})





