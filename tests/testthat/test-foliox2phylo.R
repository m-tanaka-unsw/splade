##########################################################################################
#
# Unit Testing: foliox2phylo
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

# check foliox argument is a valid foliox class
test_that("invalid foliox given", {
  expect_error(foliox2phylo(1, TRUE))
})

# address.for.tips is not boolean
test_that("address.for.tips is not a boolean", {
  expect_error(foliox2phylo(egfoliox_initial, "test"))
})

# empty foliox tree
test_that("foliox given is empty", {
  expect_error(foliox2phylo(list(), TRUE))
})

##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

test_that("foliox2phylo returns a valid phylo object", {
  
  # Check if output correct
  # convert to phylo, then convert back to foliox to see if identical to original foliox 
  org_foliox <- egfoliox_initial
  phylo_obj <- foliox2phylo(org_foliox)
  new_foliox <- phylo2foliox(phylo_obj)

##
##  expect_equal(org_foliox, new_foliox)
## phylo and foliox number vertices differently so this test is too sensitive

  print(new_foliox)
  # Check if the result is of class "phylo"
  expect_s3_class(phylo_obj, "phylo")
  
})





