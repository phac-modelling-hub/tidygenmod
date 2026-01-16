test_that("lookup table for outcomes has expected format", {
  expect_equal(names(lookup_outcome()), c("outcome", "abm", "epac"))
})
