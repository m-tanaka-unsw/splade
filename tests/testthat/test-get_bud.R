##########################################################################################
#
# Unit Testing: get_bud
# By: Monique Vith
# Supervisor: Mark Tanaka
#
##########################################################################################

##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# passing case: numeric does not contain decimal point 
test_that("numeric does not contain decimal point", {
   input <- 3
   output <- get_bud(input)
   expected <- "0"
   
   expect_equal(output, expected)
})

# passing case: numeric contains a decimal point 
test_that("numeric contains a decimal point", {
   input <- 3.1
   output <- get_bud(input)
   expected <- "1"
   
   expect_equal(output, expected)
})

# passing case: numeric cis zero
test_that("numeric is zero", {
   input <- 0
   output <- get_bud(input)
   expected <- "0"
   
   expect_equal(output, expected)
})

