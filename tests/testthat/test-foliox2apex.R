##########################################################################################
#
# Unit Testing: foliox2apex
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
  expect_error(foliox2apex(1, TRUE))
})

# second argument should be boolean TRUE/FALSE
test_that("address.for.tips is not a boolean", {
  expect_error(foliox2apex(egfoliox_initial, "test"))
})

##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# Pass: valid ape return
test_that("Correctly outputs ape object", {
  result <- foliox2apex(egfoliox_initial)
  expect_true(inherits(result, "list"))
  
})

# Correct Output 
test_that("foliox2apex returns a tree with correct number of tips and internal nodes", {
  # Extract the phylo object (the tree) and Check it is class phylo
  foliox_mock <- egfoliox_initial
  result <- foliox2apex(foliox_mock)
  apex_tree <- result$ape
  expect_s3_class(apex_tree, "phylo")
  
  # Check if result$head.label is correct 
  org_heads <- sort(as.character(foliox_mock$headv))
  apex_heads <- sort(result$head.label)
  expect_equal(org_heads, apex_heads)
  
  # Check if result$intra.label is correct (internal nodes)
  expected_internal_nodes <- sort(c("0", "1", "2",  "8",  "9",  "19", "20", "21", "26", "3",  "4",  "10", "11", "12", "30", "13"))
  output_internal_nodes <- sort(result$intr.label)
  expect_equal(expected_internal_nodes, output_internal_nodes)

  # Check if result$tip.nums is correct 
  expected_tips <- sort(c("32", "31", "5", "6", "7", "28", "27", "15", "14", "16", "17", "18", "23",  "22", "24", "25", "29", "33"))
  output_tips <- sort(result$tip.nums)
  expect_equal(expected_tips, output_tips)
  
})

