context("Check standarize call with formals")

test_that("Standarize call with formals primitive function", {
  user <- rlang::get_expr(quote(mean(1:3, na.rm = TRUE)))
  user_stand <- call_standardise_formals(user)
  
  testthat::expect_equal(user_stand,
                         call("mean", x = 1:3, na.rm = TRUE))

  user <- quote(mean(1:3, 0, TRUE))
  user_stand <- call_standardise_formals(user)
  
  testthat::expect_equal(user_stand,
                         call("mean", x = 1:3, 0, TRUE))
  
})

test_that("Standarize call with formals user function", {

  my_func <- function(x, y, z=100, a = TRUE, b = 3.14, c = "s", ...) {x + y + z + b}

  user <- rlang::get_expr(quote(my_func(x = 1, 20)))
  user_stand <- gradethis:::call_standardise_formals(user,
                                                     env = rlang::env(my_func = my_func))

  testthat::expect_equal(user_stand,
                         call("my_func", x = 1, y = 20, z = 100, a = TRUE, b = 3.14, c = "s"))

})

test_that("Standarize call with ... and kwargs", {
  
  a <- quote(vapply(list(1:3, 4:6), mean, numeric(1), 0, TRUE))
  b <- quote(vapply(list(1:3, 4:6), mean, numeric(1), trim = 0, TRUE))
  c <- quote(vapply(list(1:3, 4:6), mean, numeric(1), 0, na.rm = TRUE))
  d <- quote(vapply(list(1:3, 4:6), mean, numeric(1), trim = 0, na.rm = TRUE))
  
  xa <- quote(vapply(X = list(1:3, 4:6), FUN = mean, FUN.VALUE = numeric(1), 0, TRUE, USE.NAMES = TRUE)) # nolint
  xb <- quote(vapply(X = list(1:3, 4:6), FUN = mean, FUN.VALUE = numeric(1), trim = 0, TRUE, USE.NAMES = TRUE)) # nolint
  xc <- quote(vapply(X = list(1:3, 4:6), FUN = mean, FUN.VALUE = numeric(1), 0, na.rm = TRUE, USE.NAMES = TRUE)) # nolint
  xd <- quote(vapply(X = list(1:3, 4:6), FUN = mean, FUN.VALUE = numeric(1), trim = 0, na.rm = TRUE, USE.NAMES = TRUE)) # nolint
  
  testthat::expect_equal(call_standardise_formals(a), xa)
  testthat::expect_equal(call_standardise_formals(b), xb)
  testthat::expect_equal(call_standardise_formals(c), xc)
  testthat::expect_equal(call_standardise_formals(d), xd)
  

  # use.names of vapply in the before the ...
  a <- quote(vapply(list(1:3, 4:6), mean, numeric(1), 0, USE.NAMES = TRUE, TRUE))
  b <- quote(vapply(list(1:3, 4:6), mean, numeric(1), trim = 0, USE.NAMES = TRUE, TRUE))
  c <- quote(vapply(list(1:3, 4:6), mean, numeric(1), 0, USE.NAMES = TRUE, na.rm = TRUE))
  d <- quote(vapply(list(1:3, 4:6), mean, numeric(1), trim = 0, USE.NAMES = TRUE, na.rm = TRUE))
  
  testthat::expect_equal(call_standardise_formals(a), xa)
  testthat::expect_equal(call_standardise_formals(b), xb)
  testthat::expect_equal(call_standardise_formals(c), xc)
  testthat::expect_equal(call_standardise_formals(d), xd)
})

test_that("When an invalid function passed (i.e., corrupt language object)", {
  user <- quote(1(a(1)))
  
  testthat::expect_equal(
    call_standardise_formals(user), user)
})


test_that("Standarize call with include_defaults = FALSE", {
  library(readr)
  user <- rlang::get_expr(quote(read_csv("foo.csv", skip = 3)))
  user_stand <- gradethis:::call_standardise_formals(user)
  user_stand_mini <- gradethis:::call_standardise_formals(user,include_defaults = FALSE)
  testthat::expect_equal(    user_stand_mini,
                             call('read_csv', file = "foo.csv", skip = 3)
                             )
  testthat::expect_equal(    user_stand,
                             quote(read_csv(file = "foo.csv", col_names = TRUE, col_types = NULL, 
                                            locale = default_locale(), na = c("", "NA"), 
                                            quoted_na = TRUE, quote = "\"", comment = "", 
                                            trim_ws = TRUE, skip = 3, n_max = Inf, guess_max = min(1000, 
                                            n_max), progress = show_progress(), skip_empty_rows = TRUE))
                             )
  
  

  
})
