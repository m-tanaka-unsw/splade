test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("testing", {
  expect_error(phylo2foliox("2", "2"))
})
