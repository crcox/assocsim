test_that("ppmi is zero when N = 1 (p == rsum by definition)", {
  labels <- letters[1:10]
  p <- setNames(seq(0.1, 1.0, by = 0.1), labels)
  observed <- ppmi(p, rprob = p / sum(p))
  expected <- setNames(rep(0, 10), labels)
  expect_equal(observed, expected)
})

test_that("ppmi is one when N = 2 and responses are unique (numeric)", {
  labels <- letters[1:10]
  p <- setNames(seq(0.1, 1.0, by = 0.1), labels)
  rsum <- p / sum(p)
  rprob <- rsum / 2
  observed <- ppmi(p, rprob)
  expected <- setNames(rep(1, 10), labels)
  expect_equal(observed, expected)
})

test_that("ppmi is 2 when N = 4 and response is unique and nonzero  (matrix)", {
  X <- rbind(
    c(1, 1, 1, 0, 0, 0),
    c(0, 0, 0, 1, 1, 1),
    c(0, 0, 0, 1, 1, 1),
    c(0, 0, 0, 1, 1, 1)
  )
  colnames(X) <- letters[1:6]
  observed <- ppmi(X)[1,]
  expected <- setNames(c(2, 2, 2, 0 ,0 ,0), letters[1:6])
  expect_equal(observed, expected)
})
