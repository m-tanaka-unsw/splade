##########################################################################################
#
# Unit Testing: generate_new_bud
# By: Monique Vith
# Supervisor: Mark Tanaka
#
##########################################################################################

##########################################################################################
#
#                                 TESTING ERROR CASES
#
##########################################################################################

# error case: Invalid Vertices (head and tail) given. Must be numeric and positive. 
test_that("Invalid vertices given to generate_new_bud", {
   # invalid head and tail vertex
   expect_error(generate_new_bud("bye", "hi"))
   
   # invalid head vertex
   expect_error(generate_new_bud(-1, "hi"))
   
   # invalid tail vertex
   expect_error(generate_new_bud(-1, "hi"))
   
})



##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# passing case: When the tail and head vertex are the same 
# NB: THIS wouldn't normally occur
test_that("Given equal head and tail vertices", {
   tail <- 12
   head <- 12
   output <- generate_new_bud(tail, head)
   expected <- "12"
   
   expect_equal(output, expected)
})

# passing case: When the tail and head vertex are different
test_that("Given different head and tail vertices", {
   tail <- 12
   head <- 3
   output <- generate_new_bud(tail, head)
   expected <- "3.1"
   
   expect_equal(output, expected)
})

# passing case: When the tail is zero and head vertex is not zero
test_that("Given the tail is zero and the head is not zero", {
   tail <- 0
   head <- 3
   output <- generate_new_bud(tail, head)
   expected <- "3.1"
   
   expect_equal(output, expected)
})

# passing case: When the head is zero and tail vertex is not zero
test_that("Given the head is zero and the tail is not zero", {
   tail <- 3
   head <- 0
   output <- generate_new_bud(tail, head)
   expected <- "0.1"
   
   expect_equal(output, expected)
})


