test_that("lookup table for outcomes has expected format", {
  expect_equal(names(lookup_outcomes()), c("outcome", "abm", "epac", "outcome_label"))
})
