##########################################################################################
#
# Unit Testing: foliox_clade
# By: Monique Vith
# Supervisor: Mark Tanaka
#
##########################################################################################

library(ape)

##########################################################################################
#
#                                 TESTING ERROR CASES
#
##########################################################################################

# invalid foliox object given
test_that("foliox is invalid", {
  expect_error(foliox_clade(1, 1, TRUE))
})

# headv not in given foliox
test_that("given headv not in foliox", {
  expect_error(foliox_clade(egfoliox_initial, 40, TRUE))
})

# remove.prefix is not logical
test_that("remove.prefix is not a logical", {
  expect_error(foliox_clade(egfoliox_initial, 40, "TRUE"))
})

##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# headv is equal to 0
test_that("headv is equal to zero", {
  result <- foliox_clade(egfoliox_initial, 0, TRUE)
  expect_equal(result, egfoliox_initial)
})

# return type is foliox
test_that("return type is valid", {
  expect_true(inherits(foliox_clade(egfoliox_initial, 2, TRUE), "foliox"))
})


test_that("correctly extracts clade when headv is in the middle of the tree and remove.prefix is TRUE", {
  headv <- 2
  result <- foliox_clade(egfoliox_initial, headv)

  # headv should be first index in headv list
  expect_equal(result$headv[1], headv)

  # the beginning of the address should always be headv
  first_numbers <- sapply(result$address, function(x) strsplit(x, "-")[[1]][1])
  expect_true(all(first_numbers == "2"))
})

test_that("correctly extracts clade when headv is the tip of the tree and remove.prefix is TRUE", {
  headv <- 5
  result <- foliox_clade(egfoliox_initial, headv)

  # headv should be first index in headv list
  expect_equal(result$headv[1], headv)

  # should only be one node present
  expect_equal(length(result$address), 1)
  expect_equal(length(result$headv), 1)
})


# check distances between root node to every tip and compare it to original foliox
test_that("distances between root node to every tip in the clade is the same as the distances in original foliox", {
  headv <- 19
  result <- foliox_clade(egfoliox_initial, headv)
  
  org_tip1 <-distance_between(headv, 23, egfoliox_initial)
  org_tip2 <-distance_between(headv, 22, egfoliox_initial)
  org_tip3 <-distance_between(headv, 24, egfoliox_initial)
  org_tip4 <-distance_between(headv, 25, egfoliox_initial)
  
  new_tip1 <-distance_between(headv, 23, result)
  new_tip2 <-distance_between(headv, 22, result)
  new_tip3 <-distance_between(headv, 24, result)
  new_tip4 <-distance_between(headv, 25, result)
  
  expect_equal(org_tip1, new_tip1)
  expect_equal(org_tip2, new_tip2)
  expect_equal(org_tip3, new_tip3)
  expect_equal(org_tip4, new_tip4)
})

