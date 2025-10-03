##########################################################################################
#
# Integration Testing: add_a_leaf_to_foliox
# By: Monique Vith
# Supervisor: Mark Tanaka
#
##########################################################################################

##########################################################################################
#
#                                 TESTING ERROR CASES
#
##########################################################################################

# Error: Invalid headv argument
test_that("Test should throw an error when Headv is not numeric and greater than 0", {
  # headv is a character not numeric
  expect_error(add_a_leaf_to_foliox("10", egfoliox_initial, "new_taxon", 0.02, 0.03))

  # headv is less than 1
  expect_error(add_a_leaf_to_foliox(-1, egfoliox_initial, "new_taxon", 0.02, 0.03))

  # headv is the root node
  expect_error(add_a_leaf_to_foliox(0, egfoliox_initial, "new_taxon", 0.02, 0.03))
})

# Error: foliox is not the correct class
test_that("Test should throw an error if foliox object given is not of class foliox", {
  expect_error(add_a_leaf_to_foliox(1, "this is not of foliox class", "new_taxon", 0.02, 0.03))
})

# Error: leafname is not correct class
test_that("Test should throw an error if leafname is not of class character", {
  # leafname is not character
  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, 1, 0.02, 0.03))
})

# Error: bhat is not correct class and/or not positive
test_that("Test should throw error if bhat object is not of class numeric and positive", {
  # bhat is not numeric
  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, "new_taxon", "not numeric", 0.03, 0.04))

  # bhat is negative
  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, "new_taxon", -1, 0.03, 0.04))
})

# Error: chat is not correct class and/or not positive
test_that("Test should throw error if chat object is not of class numeric and positive", {
  # chat is not numeric
  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, "new_taxon", 0.02, "0.03"))

  # chat is negative
  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, "new_taxon", 0.02, -1))
})

# Error: emc is not correct class and/or not positive
#test_that("Test should throw error if emc object is not of class numeric and positive", {
  # emc is not numeric
#  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, "new_taxon", 0.02, 0.03, "0.04"))

  # emc is negative
#  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, "new_taxon", 0.02, 0.03, -1))
#})

# Error: not enough arguments given
test_that("Test there are enough arguments give", {
  expect_error(add_a_leaf_to_foliox(1,2,3))
  expect_error(add_a_leaf_to_foliox(1,2,3,4,5,6,7))
})

# Error: invalid emc and chat provided
test_that("Test the function returns an error given invalid emc and chat", {
  # Given large emc value
#  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, "new_taxon", 0.02, 0.03, 11))

  # Given large chat value
  expect_error(add_a_leaf_to_foliox(1, egfoliox_initial, "new_taxon", 0.02, 11))

})

# Error: given headv is not inside of given foliox
test_that("Test should throw error if given headv is not in foliox", {
  expect_error(add_a_leaf_to_foliox(55, egfoliox_initial, "tip_test", 0.1, 0.01))
})

######################################################################
#
#                    TESTING PASSING CASES
#
######################################################################

# Pass: valid folio return
test_that("Correctly outputs foliox object", {
  headv <- 32
  bhat <- 0.1
  chat <- 0.01
  emc <- 0.0759205603220433
  result <- add_a_leaf_to_foliox(headv, egfoliox_initial, "tip_test", bhat, chat)  # the length of the edge 32 is 0.0859205603220433
  
  expect_true(inherits(result, "foliox"))
  
})

# Pass: Add a leaf to the foliox in the middle of the edge
test_that("test the function correctly adds a leaf to given foliox in the middle of edge", {
  original <- egfoliox_initial
  headv <- 32
  bhat <- 0.1
  chat <- 0.01
  emc <- 0.0759205603220433
  leaf_added <- add_a_leaf_to_foliox(headv, egfoliox_initial, "tip_test", bhat, chat) # the length of the edge 32 is 0.0859205603220433
  leaf_added$headv <- as.numeric(leaf_added$headv)

  # Test name is correctly added
  name_original_len <- length(original$name) # should be 18
  name_leaf_added_len <- length(leaf_added$name) # should be 19 - this needs to have one more character than original, that last character needs to equal the leaf_name
  expect_equal(name_leaf_added_len, name_original_len + 1)
  expect_equal(leaf_added$name[name_original_len + 1], "tip_test")
  expect_equal(leaf_added$name[1:name_original_len], original$name[1:name_original_len])

  # test address is correctly added
  addr_original_len <- length(original$address) # should be 18
  addr_leaf_added_len <- length(leaf_added$address) # should be 19 - this needs to have one more character than original, that last character needs to equal correct address
  expect_equal(addr_leaf_added_len, addr_original_len + 1)
  expect_equal(leaf_added$address[1:addr_original_len], original$address[1:addr_original_len]) # Ensure the first 18 indexes are the same
  expect_equal(leaf_added$address[addr_leaf_added_len], "30-32.1-34")

  # test headv is correctly added - index 34 should be the new bud added, index 35 should be index 33 + 1
  headv_original_len <- length(original$headv) # should be 33
  headv_leaf_added_len <- length(leaf_added$headv) # should be 35
  expect_equal(headv_leaf_added_len, headv_original_len + 2)
  expect_equal(leaf_added$headv[headv_leaf_added_len], leaf_added$headv[headv_original_len] + 1)
  expect_equal(leaf_added$headv[1:headv_original_len], original$headv[1:headv_original_len]) # Ensure the first 33 indexes are the same
  expect_equal(leaf_added$headv[headv_leaf_added_len - 1], 32.1)
  expect_equal(leaf_added$headv[headv_leaf_added_len], 34.0)

  # test weight is correctly added - 34 should be chat, 35 should be bhat
  weight_original <- length(original$weight) # should be 33
  weight_leaf_added <- length(leaf_added$weight) # should be 35
  expect_equal(weight_leaf_added, weight_original + 2)
  expect_equal(leaf_added$headv[1:33], original$headv[1:33]) # Ensure the first 33 indexes are the same
  expect_equal(leaf_added$weight[weight_leaf_added], bhat)
  expect_equal(leaf_added$weight[weight_original + 1], chat)

})

# Pass: Add a leaf to the foliox at the tail of an edge
test_that("test the function correctly adds a leaf to given foliox at the tail of edge", {
  original <- egfoliox_initial

  headv <- 32
  bhat <- 0.1
  chat <- 0
  emc <- 0.0859205603220433
  leaf_added <- add_a_leaf_to_foliox(headv, egfoliox_initial, "tip_test", bhat, chat) # the length of the edge 32 is 0.0859205603220433
  leaf_added$headv <- as.numeric(leaf_added$headv)

  # Test name is correctly added
  name_original_len <- length(original$name) # should be 18
  name_leaf_added_len <- length(leaf_added$name) # should be 19 - this needs to have one more character than original, that last character needs to equal the leaf_name
  expect_equal(name_leaf_added_len, name_original_len + 1)
  expect_equal(leaf_added$name[name_original_len + 1], "tip_test")
  expect_equal(leaf_added$name[1:name_original_len], original$name[1:name_original_len])

  # test address is correctly added
  addr_original_len <- length(original$address) # should be 18
  addr_leaf_added_len <- length(leaf_added$address) # should be 19 - this needs to have one more character than original, that last character needs to equal correct address
  expect_equal(addr_leaf_added_len, addr_original_len + 1)
  expect_equal(leaf_added$address[1:addr_original_len], original$address[1:addr_original_len]) # Ensure the first 18 indexes are the same
  expect_equal(leaf_added$address[addr_leaf_added_len], "30-34")

  # test headv is correctly added - index 34 should be the new bud added, index 35 should be index 33 + 1
  headv_original_len <- length(original$headv) # should be 33
  headv_leaf_added_len <- length(leaf_added$headv) # should be 35
  expect_equal(headv_leaf_added_len, headv_original_len + 1)
  expect_equal(leaf_added$headv[headv_leaf_added_len], leaf_added$headv[headv_original_len] + 1)
  expect_equal(leaf_added$headv[1:headv_original_len], original$headv[1:headv_original_len]) # Ensure the first 33 indexes are the same
  expect_equal(leaf_added$headv[headv_leaf_added_len], 34)

  # test weight is correctly added
  last_index_og <- length(original$weight)
  last_index <- length(leaf_added$weight)
  expect_equal(leaf_added$weight[last_index], bhat)
  expect_equal(last_index_og + 1, last_index)

})

# Pass: Add a leaf to the foliox at the head of an edge
test_that("test the function correctly adds a leaf to given foliox at the head of edge", {
  original <- egfoliox_initial

  headv <- 32
  bhat <- 0.1
  chat <- 0.0859205603220433
  emc <- 0
  leaf_added <- add_a_leaf_to_foliox(headv, egfoliox_initial, "tip_test", bhat, chat) # the length of the edge 32 is 0.0859205603220433
  leaf_added$headv <- as.numeric(leaf_added$headv)

  # Test name is correctly added
  name_original_len <- length(original$name) # should be 18
  name_leaf_added_len <- length(leaf_added$name) # should be 19 - this needs to have one more character than original, that last character needs to equal the leaf_name
  expect_equal(name_leaf_added_len, name_original_len + 1)
  expect_equal(leaf_added$name[name_original_len + 1], "tip_test")
  expect_equal(leaf_added$name[1:name_original_len], original$name[1:name_original_len])

  # test address is correctly added
  addr_original_len <- length(original$address) # should be 18
  addr_leaf_added_len <- length(leaf_added$address) # should be 19 - this needs to have one more character than original, that last character needs to equal correct address
  expect_equal(addr_leaf_added_len, addr_original_len + 1)
  expect_equal(leaf_added$address[1:addr_original_len], original$address[1:addr_original_len]) # Ensure the first 18 indexes are the same
  expect_equal(leaf_added$address[addr_leaf_added_len], "30-32-34")

  # test headv is correctly added - index 34 should be the new bud added, index 35 should be index 33 + 1
  headv_original_len <- length(original$headv) # should be 33
  headv_leaf_added_len <- length(leaf_added$headv) # should be 35
  expect_equal(headv_leaf_added_len, headv_original_len + 1)
  expect_equal(leaf_added$headv[headv_leaf_added_len], leaf_added$headv[headv_original_len] + 1)
  expect_equal(leaf_added$headv[1:headv_original_len], original$headv[1:headv_original_len])
  expect_equal(leaf_added$headv[headv_leaf_added_len], 34)

  # test weight is correctly added
  last_index_og <- length(original$weight)
  last_index <- length(leaf_added$weight)
  expect_equal(leaf_added$weight[last_index], bhat)
  expect_equal(last_index_og + 1, last_index)

})

# Pass: add a leaf, then add a leaf again on a different edge of the same foliox
test_that("Test when a leaf is added, then added again on a different edge on the same foliox", {
  original <- egfoliox_initial

  headv <- 32
  bhat <- 0.1
  chat <- 0.01
  emc <- 0.0759205603220433
  leaf_added <- add_a_leaf_to_foliox(headv, egfoliox_initial, "test", bhat, chat) # the weight of the edge 32 is 0.0859205603220433
  leaf_added$headv <- as.numeric(leaf_added$headv)

  headv2 <- 8
  bhat2 <- 0.1
  chat2 <- 0.01
  emc2 <- 0.0125969268146228
  leaf_added_2 <- add_a_leaf_to_foliox(headv2, leaf_added, "test_2", bhat2, chat2) # the weight of edge 8 is 0.0225969268146228
  leaf_added_2$headv <- as.numeric(leaf_added_2$headv)

  # test name is correctly added
  name_original_len <- length(original$name) # should be 18
  name_leaf_added_len <- length(leaf_added_2$name) # should be 20 - this needs to have one more character than original, that last character needs to equal the leaf_name
  expect_equal(name_leaf_added_len, name_original_len + 2)
  expect_equal(leaf_added_2$name[name_leaf_added_len - 1], "test")
  expect_equal(leaf_added_2$name[name_leaf_added_len], "test_2")
  expect_equal(leaf_added_2$name[1:name_original_len], original$name[1:name_original_len]) # Ensure the first 18 indexes are the same

  # test address is correctly added
  addr_original_len <- length(original$address) # should be 18
  addr_leaf_added_len <- length(leaf_added_2$address) # should be 20 - 2 new addresses added
  expect_equal(addr_leaf_added_len, addr_original_len + 2)
  expect_equal(leaf_added_2$address[1:addr_original_len], original$address[1:addr_original_len]) # Ensure the first 18 indexes are the same
  expect_equal(leaf_added_2$address[addr_leaf_added_len - 1], "30-32.1-34")
  expect_equal(leaf_added_2$address[addr_leaf_added_len], "1-2-8.1-35")

  # test headv is correctly added - index 34 should be the new bud added, index 35 should be index 33 + 1
  headv_original_len <- length(original$headv) # should be 33
  headv_leaf_added_len <- length(leaf_added_2$headv) # should be 37
  expect_equal(headv_leaf_added_len, headv_original_len + 4)
  expect_equal(leaf_added_2$headv[headv_original_len + 2], leaf_added_2$headv[headv_original_len] + 1) # checks index 35
  expect_equal(leaf_added_2$headv[headv_leaf_added_len], leaf_added_2$headv[headv_original_len] + 2) # checks index 37
  expect_equal(leaf_added$headv[1:headv_original_len], original$headv[1:headv_original_len]) # Ensure the first 33 indexes are the same
  expect_equal(leaf_added_2$headv[headv_leaf_added_len - 3], 32.1)
  expect_equal(leaf_added_2$headv[headv_leaf_added_len - 2], 34.0)
  expect_equal(leaf_added_2$headv[headv_leaf_added_len - 1], 8.1)
  expect_equal(leaf_added_2$headv[headv_leaf_added_len], 35.0)

  # test weight is correctly added - 34 should be chat, 35 should be bhat
  weight_original <- length(original$weight) # should be 33
  weight_leaf_added <- length(leaf_added_2$weight) # should be 37
  expect_equal(weight_leaf_added, weight_original + 4)
  expect_equal(leaf_added_2$headv[1:33], original$headv[1:33]) # Ensure the first 33 indexes are the same
  bhat_1 <- 0.1
  bhat_2 <- 0.1
  expect_equal(leaf_added_2$weight[weight_leaf_added], bhat_2) # index 37
  expect_equal(leaf_added_2$weight[weight_leaf_added - 2], bhat_1) # index 35
  chat_1 <- 0.01
  chat_2 <- 0.01
  expect_equal(leaf_added_2$weight[weight_original + 1], chat_1) # index 34
  expect_equal(leaf_added_2$weight[weight_original + 3], chat_2) # index 36
})

# Pass: add a leaf, then add a leaf to the same edge in the middle of the same foliox
test_that("Test when leaf is added twice to the same edge of the same foliox", {
  original <- egfoliox_initial

  headv <- 32
  bhat <- 0.1
  chat <- 0.01
  emc <- 0.0759205603220433
  leaf_added <- add_a_leaf_to_foliox(headv, egfoliox_initial, "test_1", bhat, chat) # the weight of the edge 32 is 0.0859205603220433
  leaf_added$headv <- as.numeric(leaf_added$headv)

  headv2 <- 32
  bhat2 <- 0.1
  chat2 <- 0.07
  emc2 <- 0.0059205603220433
  leaf_added_2 <- add_a_leaf_to_foliox(headv2, leaf_added, "test_2", bhat2, chat2) # the weight of edge 8 is 0.0225969268146228
  leaf_added_2$headv <- as.numeric(leaf_added_2$headv)

  # Test correct name
  last_index_og <- length(original$name)
  last_index <- length(leaf_added_2$name)
  expect_equal(last_index_og + 2, last_index)
  expect_equal(leaf_added_2$name[last_index], "test_2")
  expect_equal(leaf_added_2$name[last_index - 1], "test_1")

  # Test correct address
  last_index_og <- length(original$address)
  last_index <- length(leaf_added_2$address)
  expect_equal(last_index_og + 2, last_index)
  expect_equal(leaf_added_2$address[last_index], "30-32.01-35")
  expect_equal(leaf_added_2$address[last_index - 1], "30-32.1-34")

  # Test correct headv
  last_index_og <- length(original$headv)
  last_index <- length(leaf_added_2$headv)
  expect_equal(last_index_og + 4, last_index)
  expect_equal(leaf_added_2$headv[last_index], 35)
  expect_equal(leaf_added_2$headv[last_index - 1], 32.01)
  expect_equal(leaf_added_2$headv[last_index - 2], 34)
  expect_equal(leaf_added_2$headv[last_index - 3], 32.1)

  # Test correct weight
  last_index_og <- length(original$weight)
  last_index <- length(leaf_added_2$weight)
  expect_equal(last_index_og + 4, last_index)
  expect_equal(leaf_added_2$weight[last_index], bhat2)
  expect_equal(leaf_added_2$weight[last_index - 1], chat2)
  expect_equal(leaf_added_2$weight[last_index - 2], bhat)
  expect_equal(leaf_added_2$weight[last_index - 3], chat)

})

# Pass: add a leaf, then add a leaf to the same edge in the head of the same foliox
test_that("leaves added to the same pendant edge; second time at the head (tip)", {
  original <- egfoliox_initial

  headv <- 32
  bhat <- 0.1
  chat <- 0.01
  emc <- 0.0759205603220433
  leaf_added <- add_a_leaf_to_foliox(headv, egfoliox_initial, "test_1", bhat, chat) # the weight of the edge 32 is 0.0859205603220433
  leaf_added$headv <- as.numeric(leaf_added$headv)

  headv2 <- 32
  bhat2 <- 0.1
  chat2 <- 0.0759205603220433
  emc2 <- 0
  leaf_added_2 <- add_a_leaf_to_foliox(headv2, leaf_added, "test_2", bhat2, chat2) # the weight of edge 8 is 0.0225969268146228
  leaf_added_2$headv <- as.numeric(leaf_added_2$headv)

  # Test correct name
  last_index_og <- length(original$name)
  last_index <- length(leaf_added_2$name)
  expect_equal(last_index_og + 2, last_index)
  expect_equal(leaf_added_2$name[last_index], "test_2")
  expect_equal(leaf_added_2$name[last_index - 1], "test_1")

  # Test correct address
  last_index_og <- length(original$address)
  last_index <- length(leaf_added_2$address)
  expect_equal(last_index_og + 2, last_index)
#  expect_equal(leaf_added_2$address[last_index - 1], "30-32-34") #<--- orig
  expect_equal(leaf_added_2$address[last_index - 1], "30-32.1-34") #<-- corrected
  expect_equal(leaf_added_2$address[last_index], "30-32-35")

  # Test correct headv
  last_index_og <- length(original$headv)
  last_index <- length(leaf_added_2$headv)
  expect_equal(last_index_og + 3, last_index)
  expect_equal(leaf_added_2$headv[last_index], 35)
  expect_equal(leaf_added_2$headv[last_index - 1], 34)
  expect_equal(leaf_added_2$headv[last_index - 2], 32.1)

  # Test correct weight
  last_index_og <- length(original$weight)
  last_index <- length(leaf_added_2$weight)
  expect_equal(last_index_og + 3, last_index)
  expect_equal(leaf_added_2$weight[last_index], bhat2)
  expect_equal(leaf_added_2$weight[last_index - 1], bhat)
  expect_equal(leaf_added_2$weight[last_index - 2], chat)

})

# Pass: add a leaf, then add a leaf to the same edge in the tail of the same foliox
test_that("leaves added to the same pendant edge; second time at the tail.", {
  original <- egfoliox_initial

  headv <- 32
  bhat <- 0.1
  chat <- 0.01
  emc <- 0.0759205603220433
  leaf_added <- add_a_leaf_to_foliox(headv, egfoliox_initial, "test_1", bhat, chat) # the weight on edge 32 is 0.0859205603220433
  leaf_added$headv <- as.numeric(leaf_added$headv)

  headv2 <- 32
  bhat2 <- 0.1
  chat2 <- 0
  emc2 <- 0.0759205603220433
  leaf_added_2 <- add_a_leaf_to_foliox(headv2, leaf_added, "test_2", bhat2, chat2) #
  leaf_added_2$headv <- as.numeric(leaf_added_2$headv)

  # Test correct name
  last_index_og <- length(original$name)
  last_index <- length(leaf_added_2$name)
  expect_equal(last_index_og + 2, last_index)
  expect_equal(leaf_added_2$name[last_index], "test_2")
  expect_equal(leaf_added_2$name[last_index - 1], "test_1")

  # Test correct address
  last_index_og <- length(original$address)
  last_index <- length(leaf_added_2$address)
  expect_equal(last_index_og + 2, last_index)
  expect_equal(leaf_added_2$address[last_index - 1], "30-32.1-34")
  expect_equal(leaf_added_2$address[last_index], "30-32.1-35")

  # Test correct headv
  last_index_og <- length(original$headv)
  last_index <- length(leaf_added_2$headv)
  expect_equal(last_index_og + 3, last_index)
  expect_equal(leaf_added_2$headv[last_index], 35)
  expect_equal(leaf_added_2$headv[last_index - 1], 34)
  expect_equal(leaf_added_2$headv[last_index - 2], 32.1)

  # Test correct weight
  last_index_og <- length(original$weight)
  last_index <- length(leaf_added_2$weight)
  expect_equal(last_index_og + 3, last_index)
  expect_equal(leaf_added_2$weight[last_index], bhat2)
  expect_equal(leaf_added_2$weight[last_index - 1], bhat)
  expect_equal(leaf_added_2$weight[last_index - 2], chat)

})

