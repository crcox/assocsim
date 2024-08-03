test_that("ppmi is zero when N = 1 (p == rsum by definition)", {
  labels <- letters[1:10]
  p <- setNames(seq(0.1, 1.0, by = 0.1), labels)
  observed <- ppmi(p, rsum = p, N = 1)
  expected <- setNames(rep(0, 10), labels)
  expect_equal(observed, expected)
})

test_that("ppmi is one when N = 2 and responses are unique (p == rsum)", {
  labels <- letters[1:10]
  p <- setNames(seq(0.1, 1.0, by = 0.1), labels)
  observed <- ppmi(p, rsum = p, N = 2)
  expected <- setNames(rep(1, 10), labels)
  expect_equal(observed, expected)
})

test_that("ppmi is one when N = 4 and responses are shared by two cues (p == rsum*2)", {
  labels <- letters[1:10]
  p <- setNames(seq(0.1, 1.0, by = 0.1), labels)
  observed <- ppmi(p, rsum = p * 2, N = 4)
  expected <- setNames(rep(1, 10), labels)
  expect_equal(observed, expected)
})
