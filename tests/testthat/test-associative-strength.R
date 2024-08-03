test_that("orthogonal response sets return zero strength", {
  x <- list(
    a = c(y = .2),
    b = c(z = .3)
  )
  expect_equal(astrength(c(1, 2), x), 0)
})

test_that("identical singular response sets return one strength", {
  x <- list(
    a = c(y = 1),
    b = c(y = 1)
  )
  expect_equal(astrength(c(1, 2), x), 1)
})

test_that("identical response sets return one strength", {
  x <- list(
    a = c(w = .2, y = .3, z = .5),
    b = c(w = .2, y = .3, z = .5)
  )
  expect_equal(astrength(c(1, 2), x), 1)
})

test_that("identical response sets return one strength (data frame)", {
  x <- expand.grid(cue = c("a", "b", "c"), response = c("w", "y", "z"))
  x$R123.Strength <- rep(c(.2, .3, .5), each = 3)
  expect_equal(associative_strength(x), c(1, 1, 1))
})

test_that("identical response sets return one strength (data frame, ppmi)", {
  x <- rbind(
    expand.grid(cue = c("a", "b"), response = c("u", "v", "w")),
    expand.grid(cue = c("c", "d"), response = c("x", "y", "z"))
  )
  x$R123.Strength <- c(.2, .2, .3, .3, .5, .5, .2, .2, .3, .3, .5, .5)
  expect_equal(associative_strength(x, rescale = "ppmi"), c(1, 1, 1))
})
