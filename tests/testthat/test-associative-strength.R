test_that("orthogonal response sets return zero strength", {
  x <- list(
    a = c(y = .2),
    b = c(z = .3)
  )
  expected <- 0
  actual <- cosine_similarity(x$a, x$b)
  expect_equal(actual, expected)
})

test_that("orthogonal response sets return zero strength (full structure)", {
  x <- list(
    a = c(y = .2),
    b = c(z = .3)
  )
  expected <- structure(
    0,
    Size = length(x),
    Labels = names(x),
    Diag = FALSE,
    Upper = FALSE,
    call = NULL,
    method = "cosine",
    class = "similarity"
  )
  actual <- cosine_similarity(x)
  attr(actual, "call") <- NULL
  expect_equal(actual, expected)
})

test_that("identical singular response sets return one strength", {
  x <- list(
    a = c(y = 1),
    b = c(y = 1)
  )
  expected <- 1
  actual <- cosine_similarity(x$a, x$b)
  expect_equal(actual, expected)
})

test_that("identical response sets return one strength", {
  x <- list(
    a = c(w = .2, y = .3, z = .5),
    b = c(w = .2, y = .3, z = .5)
  )
  expected <- 1
  actual <- cosine_similarity(x$a, x$b)
  expect_equal(actual, expected)
})

test_that("identical response sets return one strength (data frame)", {
  x <- expand.grid(cue = c("a", "b", "c"), response = c("w", "y", "z"))
  x$R123.Strength <- rep(c(.2, .3, .5), each = 3)
  expected <- structure(
    c(1, 1, 1),
    Size = 3,
    Labels = c("a", "b", "c"),
    Diag = FALSE,
    Upper = FALSE,
    call = NULL,
    method = "cosine",
    class = "similarity"
  )
  actual <- cosine_similarity(x, value_var = "R123.Strength")
  attr(actual, "call") <- NULL
  expect_equal(actual, expected)
})

test_that("identical response sets return one strength (data frame, ppmi)", {
  x <- rbind(
    expand.grid(cue = c("a", "b"), response = c("u", "v", "w")),
    expand.grid(cue = c("c", "d"), response = c("x", "y", "z"))
  )
  x$R123.Strength <- c(.2, .2, .3, .3, .5, .5, .2, .2, .3, .3, .5, .5)
  expected <- structure(
    c(1,0,0,0,0,1),
    Size = 4,
    Labels = c("a", "b", "c", "d"),
    Diag = FALSE,
    Upper = FALSE,
    call = NULL,
    method = "cosine",
    class = "similarity"
  )
  actual <- x |>
    ppmi(value_var = "R123.Strength", as_list = TRUE) |>
    cosine_similarity()
  attr(actual, "call") <- NULL
  expect_equal(actual, expected)
})
