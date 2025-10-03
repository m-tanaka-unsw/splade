# ##########################################################################################
# #
# # Integration Testing: get_fi_fj
# # By: Monique Vith
# # Supervisor: Mark Tanaka
# #
# ##########################################################################################
# 
# ##########################################################################################
# #
# #                                 TESTING ERROR CASES
# #
# ##########################################################################################
# 
# # check foliox argument is a valid foliox class
# test_that("invalid foliox given", {
#   expect_error(get_fi_fj(2, "not valid"))
# })
# 
# # provided edge is not in foliox
# test_that("given edge not in foliox", {
#   expect_error(get_fi_fj(50, egfoliox_initial))
# })
# 
# # invalid edge
# test_that("invalid edge given", {
# 
#   # edge is not a numeric
#   expect_error(get_fi_fj("hello", egfoliox_initial))
# 
#   # edge is not greater than 0
#   expect_error(get_fi_fj(0, egfoliox_initial))
# })
# 
# ##########################################################################################
# #
# #                                 TESTING PASSING CASES
# #
# ##########################################################################################
# 
# test_that("correct functionality", {
#   foliox <- egfoliox_initial
# 
#   result_fi <- get_fi_fj(1, foliox)$fi
#   result_fj <- get_fi_fj(1, foliox)$fj
# 
#   expected_fi <- c(0.44066710, 0.22109870, 0.16879274, 0.03396336, 0.22635337, 0.30048822, 0.25347522, 0.45643514, 0.18914273, 0.20794570, 0.32471840, 0.29876252, 0.43054735, 0.26906853, 0.31769795)
#   expected_fj <- c(0.04503279, 0.24962439, 0.18003745)
# 
#   expect_equal(result_fi, expected_fi)
#   expect_equal(result_fj, expected_fj)
# 
# })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
