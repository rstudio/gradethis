context("Check standarize call with formals")

test_that("Standarize call with formals built-in function", {
  user <- rlang::get_expr(quote(mean(1:3, na.rm = TRUE)))
  user_stand <- gradethis:::call_standardise_formals(user)

  testthat::expect_equal(user_stand,
                         call("mean", x = 1:3, na.rm = TRUE))

})

test_that("Standarize call with formals user function", {

  my_func <- function(x, y, z=100, a = TRUE, b = 3.14, c = "s", ...) {
    x + y + z + b
  }

  user <- rlang::get_expr(quote(my_func(x = 1, 20)))
  user_stand <- gradethis:::call_standardise_formals(user,
                                                     env = rlang::env(my_func = my_func))

  testthat::expect_equal(user_stand,
                         call("my_func", x = 1, y = 20, z = 100, a = TRUE, b = 3.14, c = "s"))

})
