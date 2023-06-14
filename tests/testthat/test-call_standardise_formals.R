test_that("Standarize call with formals S3 function", {
  user <- rlang::get_expr(quote(mean(1:3, na.rm = TRUE)))
  user_stand <- call_standardise_formals(user)

  expect_equal(user_stand, quote(mean(x = 1:3, trim = 0, na.rm = TRUE)))

  user <- quote(mean(1:3, 0, TRUE))
  user_stand <- call_standardise_formals(user)

  expect_equal(user_stand, quote(mean(x = 1:3, trim = 0, na.rm = TRUE)))
})

test_that("Standarize call with formals user function", {
  my_func <- function(x, y, z = 100, a = TRUE, b = 3.14, c = "s", ...) {
    x + y + z + b
  }

  user <- rlang::get_expr(quote(my_func(x = 1, 20)))
  user_stand <- call_standardise_formals(
    user,
    env = rlang::env(my_func = my_func)
  )

  testthat::expect_equal(
    user_stand,
    quote(my_func(x = 1, y = 20, z = 100, a = TRUE, b = 3.14, c = "s"))
  )
})

test_that("Standarize call with formals user S3 function", {
  my_func <- function(x, ...) {
    UseMethod("my_func")
  }

  my_func.numeric <- function(x, y, z = 100, ...) {
    x + y + z
  }

  my_func.character <- function(x, a, b = 3.14, c = "s", ...) {
    paste(x, a, b, c)
  }

  user_numeric <- rlang::get_expr(quote(my_func(x = 1, 20)))
  user_numeric_stand <- call_standardise_formals(
    user_numeric,
    env = rlang::env(
      my_func = my_func,
      my_func.numeric = my_func.numeric,
      my_func.character = my_func.character
    )
  )

  testthat::expect_equal(
    user_numeric_stand,
    quote(my_func(x = 1, y = 20, z = 100))
  )

  user_character <- rlang::get_expr(quote(my_func(x = "1", 20)))
  user_character_stand <- call_standardise_formals(
    user_character,
    env = rlang::env(
      my_func = my_func,
      my_func.numeric = my_func.numeric,
      my_func.character = my_func.character
    )
  )

  testthat::expect_equal(
    user_character_stand,
    quote(my_func(x = "1", a = 20, b = 3.14, c = "s"))
  )
})

test_that("Standardize call with passed ... args", {
  expect_equal(
    call_standardise_formals(quote(
      purrr::map(1, mean, 0, TRUE)
    )),
    quote(
      purrr::map(.x = 1, .f = mean, trim = 0, na.rm = TRUE, .progress = FALSE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::map(list(1:2, 1:3), mean, 0, TRUE)
    )),
    quote(
      purrr::map(.x = list(1:2, 1:3), .f = mean, trim = 0, na.rm = TRUE, .progress = FALSE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::map(1, mean, na.rm = TRUE, 0)
    )),
    quote(
      purrr::map(.x = 1, .f = mean, trim = 0, na.rm = TRUE, .progress = FALSE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::map(1, mean)
    )),
    quote(
      purrr::map(.x = 1, .f = mean, trim = 0, na.rm = FALSE, .progress = FALSE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::map(1, mean, na.rm = TRUE)
    )),
    quote(
      purrr::map(.x = 1, .f = mean, trim = 0, na.rm = TRUE, .progress = FALSE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      lapply(1, mean, 0, TRUE)
    )),
    quote(
      lapply(X = 1, FUN = mean, trim = 0, na.rm = TRUE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      sapply(1, mean, 0, TRUE)
    )),
    quote(
      sapply(X = 1, FUN = mean, trim = 0, na.rm = TRUE, simplify = TRUE, USE.NAMES = TRUE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      vapply(1, mean, 0, TRUE, FUN.VALUE = numeric(1))
    )),
    quote(
      vapply(X = 1, FUN = mean, FUN.VALUE = numeric(1), trim = 0, na.rm = TRUE, USE.NAMES = TRUE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::map2(1, 0, mean, TRUE)
    )),
    quote(
      purrr::map2(.x = 1, .y = 0, .f = mean, na.rm = TRUE, .progress = FALSE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::map2(list(1:2, 1:3), list(1:2, 1:3), mean, TRUE)
    )),
    quote(
      purrr::map2(
        .x = list(1:2, 1:3),
        .y = list(1:2, 1:3),
        .f = mean,
        na.rm = TRUE,
        .progress = FALSE
      )
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::imap(c("0" = 1), mean, TRUE)
    )),
    quote(
      purrr::imap(.x = c("0" = 1), .f = mean, na.rm = TRUE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::lmap(1, mean, 0, TRUE)
    )),
    quote(
      purrr::lmap(.x = 1, .f = mean, trim = 0, na.rm = TRUE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::pmap(list(1, 0), mean, TRUE)
    )),
    quote(
      purrr::pmap(.l = list(1, 0), .f = mean, na.rm = TRUE, .progress = FALSE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::pmap(list(list(1:2, 1:3), list(1:2, 1:3)), mean, TRUE)
    )),
    quote(
      purrr::pmap(
        .l = list(list(1:2, 1:3), list(1:2, 1:3)),
        .f = mean,
        na.rm = TRUE,
        .progress = FALSE
      )
    )
  )

  a <- quote(vapply(list(1:3, 4:6), mean, numeric(1), 0, TRUE))
  b <- quote(vapply(list(1:3, 4:6), mean, numeric(1), trim = 0, TRUE))
  c <- quote(vapply(list(1:3, 4:6), mean, numeric(1), 0, na.rm = TRUE))
  d <- quote(vapply(list(1:3, 4:6), mean, numeric(1), trim = 0, na.rm = TRUE))

  xd <- quote(vapply(X = list(1:3, 4:6), FUN = mean, FUN.VALUE = numeric(1), trim = 0, na.rm = TRUE, USE.NAMES = TRUE)) # nolint

  testthat::expect_equal(call_standardise_formals(a), xd)
  testthat::expect_equal(call_standardise_formals(b), xd)
  testthat::expect_equal(call_standardise_formals(c), xd)
  testthat::expect_equal(call_standardise_formals(d), xd)

  # use.names of vapply in the before the ...
  a <- quote(vapply(list(1:3, 4:6), mean, numeric(1), 0, USE.NAMES = TRUE, TRUE))
  b <- quote(vapply(list(1:3, 4:6), mean, numeric(1), trim = 0, USE.NAMES = TRUE, TRUE))
  c <- quote(vapply(list(1:3, 4:6), mean, numeric(1), 0, USE.NAMES = TRUE, na.rm = TRUE))
  d <- quote(vapply(list(1:3, 4:6), mean, numeric(1), trim = 0, USE.NAMES = TRUE, na.rm = TRUE))

  testthat::expect_equal(call_standardise_formals(a), xd)
  testthat::expect_equal(call_standardise_formals(b), xd)
  testthat::expect_equal(call_standardise_formals(c), xd)
  testthat::expect_equal(call_standardise_formals(d), xd)
})

test_that("Standardize map() when .f is an index", {
  expect_equal(
    call_standardise_formals(quote(
      purrr::map(list(c(1, 2), c("a", "b")), 2)
    )),
    quote(
      purrr::map(.x = list(c(1, 2), c("a", "b")), .f = 2, .progress = FALSE)
    )
  )

  expect_equal(
    call_standardise_formals(quote(
      purrr::map(list(c(a = 1, b = 2), c(a = "a", b = "b")), "b")
    )),
    quote(
      purrr::map(
        .x = list(c(a = 1, b = 2), c(a = "a", b = "b")),
        .f = "b",
        .progress = FALSE
      )
    )
  )
})

test_that("Standardize call with ggplot2 functions", {
  skip_if_not_installed("ggplot2")
  withr::local_package("ggplot2")

  expect_equal(
    call_standardise_formals_recursive(
      quote(ggplot(mpg, aes(displ, hwy, color = class)) + geom_point()),
      include_defaults = FALSE
    ),
    quote(
      ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = class)) +
        geom_point()
    )
  )

  expect_equal(
    call_standardise_formals_recursive(
      quote(ggplot(mpg, aes(displ, hwy)) + geom_point(color = "red")),
      include_defaults = FALSE
    ),
    quote(
      ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
        geom_point(colour = "red")
    )
  )

  # Don't change `ggplot` arguments if it would lead to a name collision
  expect_equal(
    call_standardise_formals_recursive(
      quote(
        ggplot(mpg, aes(displ, hwy)) +
          geom_point(color = "red", colour = "blue")
      ),
      include_defaults = FALSE
    ),
    quote(
      ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
        geom_point(color = "red", colour = "blue")
    )
  )
})

test_that("code_feedback() standardizes arguments", {
  expect_null(
    with_exercise(
      mock_this_exercise(
        .user_code = "foo(1, 2)",
        .solution_code = "foo(bar = 1, baz = 2)",
        setup_exercise = foo <- function(bar, baz) bar + baz
      ),
      code_feedback()
    )
  )

  expect_null(
    with_exercise(
      mock_this_exercise(
        .user_code = "purrr::map(1:10, foo, 2)",
        .solution_code = "purrr::map(1:10, foo, baz = 2)",
        setup_exercise = foo <- function(bar, baz) bar + baz
      ),
      code_feedback()
    )
  )

  expect_null(
    with_exercise(
      mock_this_exercise(
        .user_code = "
          foo <- function(bar, baz) bar + baz
          purrr::map(1:10, foo, 2)
        ",
        .solution_code = "
          foo <- function(bar, baz) bar + baz
          purrr::map(1:10, foo, baz = 2)
        "
      ),
      code_feedback()
    )
  )
})

test_that("When an invalid function passed (i.e., corrupt language object)", {
  user <- quote(1(a(1)))

  testthat::expect_equal(call_standardise_formals(user), user)
})

test_that("Standarize call with include_defaults = FALSE", {
  suppressPackageStartupMessages(library(purrr))
  user <- rlang::get_expr(quote(insistently(mean, quiet = TRUE)))
  user_stand <- call_standardise_formals(user)
  user_stand_mini <- call_standardise_formals(user,include_defaults = FALSE)
  testthat::expect_equal(
    user_stand_mini,
    quote(insistently(f = mean, quiet = TRUE))
  )
  testthat::expect_equal(
    user_stand,
    quote(insistently(f = mean,rate = rate_backoff(), quiet = TRUE))
  )
})

test_that("Standardize call with ambiguous partial args", {
  testthat::expect_equal(
    call_standardise_formals(quote(dunif(1, m = 1, l = TRUE))),
    quote(dunif(x = 1, log = TRUE, m = 1))
  )

  testthat::expect_equal(
    call_standardise_formals(quote(dunif(1, m = 1, m = 1))),
    quote(dunif(1, m = 1, m = 1, log = FALSE))
  )

  testthat::expect_equal(
    call_standardise_formals(quote(invalid_function(1, m = 1, l = TRUE))),
    quote(invalid_function(1, m = 1, l = TRUE))
  )
})
