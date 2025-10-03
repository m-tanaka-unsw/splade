##########################################################################################
#
# Unit Testing: optimal_fm_exhaustive
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

# error case: input values are invalid 
test_that("Input sequences is invalid and should return an error", {
   sequences.initial <- system.file("extdata","sequences-initial.txt", package="splade")
   sequences.additional <- system.file("extdata","sequences-additional-one_seq.txt", package="splade")

   curr_seq <- read.dna(sequences.initial, format="sequential")
   add_seq <- read.dna(sequences.additional, format="sequential")
   foliox <- egfoliox_initial

   # arg 2 is invalid
   expect_error(optimal_fm_exhaustive("not a file", add_seq, foliox))

   # both files are invalid
   expect_error(optimal_fm_exhaustive("not a file", "not a file", foliox))

   # invalid foliox
   expect_error(optimal_fm_exhaustive(curr_seq, add_seq, "not a foliox"))

   # all invalid inputs
   expect_error(optimal_fm_exhaustive("not a file", "not a file", "not a foliox"))
})

# error case: sequences inside of arg 1 does not match sequences present in foliox
test_that("Number of sequences inside arg 1 does not match num seq in foliox", {
   sequences.initial <- system.file("extdata","sequences-initial-test_invalid_seq.txt", package="splade")
   sequences.additional <- system.file("extdata","sequences-additional-one_seq.txt", package="splade") # 17 tips
   
   curr_seq <- read.dna(sequences.initial, format="sequential")
   add_seq <- read.dna(sequences.additional, format="sequential")
   foliox <- egfoliox_initial # 18 tips 
   
   expect_error(optimal_fm_exhaustive(curr_seq, add_seq, foliox))
   
})

# error case: more than one sequence provided in arg 2 (sequence we want to add to foliox)
## NB this wouldn't normally happen since optimal_fm_exhaustive is not user-facing
## test_that("More than 1 sequence present in arg 2", {
##    sequences.initial <- system.file("extdata","sequences-initial.txt", package="splade")
##    sequences.additional <- system.file("extdata","sequences-additional-two_seq.txt", package="splade")
   
##    curr_seq <- read.dna(sequences.initial, format="sequential")
##    add_seq <- read.dna(sequences.additional, format="sequential")
##    foliox <- egfoliox_initial # 18 tips 
   
##    expect_error(optimal_fm_exhaustive(curr_seq, add_seq, foliox))
   
## })

##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# passing case: sequence we want to add is identical to an existing sequence
test_that("Sequence we want to add is identical to an existing sequence", {
   sequences.initial <- system.file("extdata","sequences-initial.txt", package="splade")
   sequences.additional <- system.file("extdata","sequences-additional-identical.txt", package="splade")
   
   curr_seq <- read.dna(sequences.initial, format="sequential")
   add_seq <- read.dna(sequences.additional, format="sequential")
   foliox <- egfoliox_initial
   
   result <- optimal_fm_exhaustive(curr_seq, add_seq, foliox)

   expect_equal(result$opt.edge, "22")
   expect_equal(result$opt.est, c(0.00000000, 0.02006065, 0.00000000))
   expect_equal(result$minQ, 0)
})

# passing case: sequence we want to add is not identical to an existing sequence
test_that("Sequence we want to add is not identical to an existing sequence", {
   sequences.initial <- system.file("extdata","sequences-initial.txt", package="splade")
   sequences.additional <- system.file("extdata","sequences-additional-one_seq.txt", package="splade")
   
   curr_seq <- read.dna(sequences.initial, format="sequential")
   add_seq <- read.dna(sequences.additional, format="sequential")
   foliox <- egfoliox_initial
   
   result <- optimal_fm_exhaustive(curr_seq, add_seq, foliox)
   
   expect_equal(result$opt.edge, 20)
   expect_equal(result$opt.est, c(0.01193569, 0.10247745, 0.05077318))
   expect_equal(result$minQ, 0.002660515)
})
