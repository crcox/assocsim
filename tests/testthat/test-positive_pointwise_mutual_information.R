test_that("ppmi is zero when N = 1", {
  observed <- ppmi(
    p    = setNames(seq(0.1, 1.0, by = 0.1), letters[1:10]),
    rsum = setNames(seq(0.1, 1.0, by = 0.1), letters[1:10]),
    N    = 1
  )
  expected <- setNames(rep(0, 10), letters[1:10])
  expect_equal(observed, expected)
})
