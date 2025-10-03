##########################################################################################
#
# Unit Testing: distance_to_vertex
# By: Monique Vith
# Supervisor: Mark Tanaka
#
##########################################################################################

##########################################################################################
#
#                                 TESTING ERROR CASES
#
##########################################################################################

# Error case: vertex is not a numeric and/or negative
test_that("vertex is invalid", {
  # vertex is not numeric
  expect_error(distance_to_vertex("hi", egfoliox_initial))

  # vertex is negative
  expect_error(distance_to_vertex(-1, egfoliox_initial))
})

# Error case: foliox argument is an invalid class
test_that("foliox arg is invalid class", {
  expect_error(distance_to_vertex(2, "hi"))
})

# Error case: vertex is not in foliox
test_that("given vertex not in foliox", {
  expect_error(distance_to_vertex(80, egfoliox_initial))
})


##########################################################################################
#
#                                 TESTING PASSING CASES
#
##########################################################################################

# Return type is numeric
test_that("return type for distance_between is numeric", {
  results <- distance_to_vertex(3, egfoliox_initial)
  expect_type(results, "double") 
})

# Vertex is also reference node (head)
test_that("vertex is the reference node", {
  results <- distance_to_vertex(0, egfoliox_initial)
  expect_equal(results, 0)
})

# vertex is the middle vertex of th folio object
test_that("Vertex is the middle vertex of the folio object", {
  results <- distance_to_vertex(5, egfoliox_initial)
  vertex_5 <- egfoliox_initial$weight[5]
  vertex_4 <- egfoliox_initial$weight[4]
  vertex_3 <- egfoliox_initial$weight[3]
  vertex_2 <- egfoliox_initial$weight[2]
  vertex_1 <- egfoliox_initial$weight[1]
  total_distance <- vertex_5 + vertex_4 + vertex_3 + vertex_2 + vertex_1
  expect_equal(results, total_distance)
})


