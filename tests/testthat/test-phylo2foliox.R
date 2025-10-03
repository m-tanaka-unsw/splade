##########################################################################################
#
# Integration Testing: phylo2foliox
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

test_that("invalid phylo tree given", {
  # invalid phylo
  expect_error(phylo2foliox("hi"))
})

##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

test_that("Correctly outputs foliox object", {
  ape.tree <- rtree(16)
  result <- phylo2foliox(ape.tree)

  expect_true(inherits(result, "foliox"))

})

test_that("Correctly outputs foliox give phylo", {
  ape.tree <- ape::rtree(16)
  result <- phylo2foliox(ape.tree)

  expect_equal(length(result$address), 16)
})

test_that("Correctly outputs foliox give phylo with one node", {
  ape.tree <- ape::rtree(1)
  result <- phylo2foliox(ape.tree)

  expect_equal(length(result$address), 1)
})

test_that("Correctly converts between phylo to foliox and vise versa", {
  ape.tree <- ape::rtree(16)
  
  result <- phylo2foliox(ape.tree)
  new_phylo <- foliox2phylo(result)
  
  compare_phylo_objs <- all.equal.phylo(new_phylo, ape.tree)
  expect_true(compare_phylo_objs)
  
})





