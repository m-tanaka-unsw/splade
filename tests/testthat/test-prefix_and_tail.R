##########################################################################################
#
# Unit Testing: prefix_and_tail
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
test_that("invalid foliox argument given", {
  expect_error(prefix_and_tail(2, "not valid"))
})

# headv is not in given foliox
test_that("given headv argument is not in foliox", {
  expect_error(prefix_and_tail(50, egfoliox_initial))
})

# invalid head
test_that("invalid head given", {

  # head is not a numeric
  expect_error(prefix_and_tail("hello", egfoliox_initial))

  # head is not greater than 0
  expect_error(prefix_and_tail(0, egfoliox_initial))
})


##########################################################################################
#
#                                 TESTING passing CASES
#
##########################################################################################

# test when the given head is attached to the root node
test_that("given head is attached to root node", {

  result <- prefix_and_tail(1, egfoliox_initial)

  expected_prefix <- ""
  expected_tail <- 0

  expect_equal(result$prefix, expected_prefix)
  expect_equal(result$tail, expected_tail)

})

# test when given head is in the middle of the tree
test_that("given head is in the middle of the tree", {

  result <- prefix_and_tail(11, egfoliox_initial)

  expected_prefix <- "1-2-8-9-10"
  expected_tail <- 10

  expect_equal(result$prefix, expected_prefix)
  expect_equal(result$tail, expected_tail)
})

# test when given head is at the tip of the tree
test_that("given head is at the tip of the tree", {

  result <- prefix_and_tail(32, egfoliox_initial)

  expected_prefix <- "30"
  expected_tail <- 30

  expect_equal(result$prefix, expected_prefix)
  expect_equal(result$tail, expected_tail)
})



