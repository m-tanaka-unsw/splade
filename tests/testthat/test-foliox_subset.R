##########################################################################################
#
# Integration Testing: foliox_subset
# By: Monique Vith
# Supervisor: Mark Tanaka
#
##########################################################################################

##########################################################################################
#
#                                 TESTING ERROR CASES
#
##########################################################################################

# check foliox argument is a valid foliox class
test_that("invalid foliox given", {
  expect_error(foliox_subset(1, c(1,2)))
})

# invalid class for subset
test_that("invalid subset given", {
  expect_error(foliox_subset(egfoliox_initial, "hi"))
})

# subset is list, and there are invalid entries
test_that("subset is invalid type", {
  vector <- c(2,"hi","hi")
  expect_error(foliox_subset(foliox, vector))
})

# index in the vector is not in foliox
test_that("elements in vector are not in foliox", {
  vector <- c(1,8,9,20)
  foliox <- egfoliox_initial
  expect_error(foliox_subset(foliox, vector))
})

# repeated elements in vector
test_that("repeated elements in vector", {
  vector <- c(1,1,2)
  foliox <- egfoliox_initial
  expect_error(foliox_subset(foliox, vector))
})

# subset is empty
test_that("subset is empty", {
  expect_error(foliox_subset(egfoliox_initial, list()))
})


##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# Pass: valid folio return
test_that("Correctly outputs foliox object", {
  result <- foliox_subset(egfoliox_initial, c(1,16))
  expect_true(inherits(result, "foliox"))
  
})

# correctly extracts tree with subset of nodes from foliox where length of subset is 2
test_that("foliox_subset correctly creates a tree given a subset of length 2", {
  result <- foliox_subset(egfoliox_initial, c(1,16))
  
  og_tip_1 <- distance_between(1, 23, egfoliox_initial)
  og_tip_2 <- distance_between(1, 22, egfoliox_initial)
  
  new_tip_1 <- distance_between(1, 23, result)
  new_tip_2 <- distance_between(1, 22, result)
  
  expect_equal(og_tip_1, new_tip_1)
  expect_equal(og_tip_2, new_tip_2)
})

# correctly extracts tree from foliox given the subset is length 1 
test_that("foliox_subset correctly creates a tree given a subset of length 1", {
  result <- foliox_subset(egfoliox_initial, c(1))
  
  og_tip_1 <- distance_between(1, 22, egfoliox_initial)
  
  new_tip_1 <- distance_between(1, 22, result)
  
  expect_equal(og_tip_1, new_tip_1)
})





