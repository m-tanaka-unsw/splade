##########################################################################################
#
# Integration Testing: Newick2Foliox
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

test_that("Invalid Newick tree", {
  # no semicolin
  newick_tree <- "( (1,2), ((3,4),5) )"
  expect_error(newick2foliox(text=newick_tree))

  # missing parenthesis
  newick_tree <- "(1,2), (3,4),5);"
  expect_error(newick2foliox(text=newick_tree))

  # invalid file
  expect_error(newick2foliox(tree.file=hi))
})

##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

test_that("Correctly outputs foliox object", {
  newick_tree <- "( (1,2), ((3, 4), 5) );"
  result <- newick2foliox(text=newick_tree)
  expect_true(inherits(result, "foliox"))

})

test_that("should create a foliox tree given newick tree in text form", {
  newick_tree <- "( (1,2), ((3, 4), 5) );"
  result <- newick2foliox(text=newick_tree)
  
  # convert to phylo and compare phylogenies 
  new_to_phylo <- foliox2phylo(result)
  og_to_phylo <- read.tree(text = newick_tree)
  
  compare_phylo_objs <- all.equal.phylo(new_to_phylo, og_to_phylo)
  expect_true(compare_phylo_objs)
  
})

