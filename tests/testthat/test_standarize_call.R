context("Check standarise call")

# test_that("Standarise call", {
#   user <- rlang::get_expr(quote(mean(1:3)))
#   user_stand <- gradethis:::standardize_call(user)
#   
#   user
#   user_stand
#   
#   testthat::expect_equal(user_stand,
#                          call("mean", 1:3))
#   
#   
#   
#   
#   
#   
#   my_func <- function(x, y, z=100, a = TRUE, b = 3.14, c = "s", ...) {
#     x + y + z + b
#   }
#   
#   user <- rlang::get_expr(quote(my_func(x = 1, 20)))
#   user_stand <- gradethis:::standardize_call(user,
#                                              env = rlang::env(my_func = my_func))
#   
#   user
#   user_stand
#   
#   testthat::expect_equal(user_stand,
#                          call("my_func", 1, y = 20, z = 100, a = TRUE, b = 3.14, c = "s"))
#   
#   
#   
#   
# })