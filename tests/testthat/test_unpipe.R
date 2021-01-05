context("Unpipe pipes")

test_that("unpipe() strips off the top level of piping", {
  pipe <- quote(iris %>% filter(Species == "Virginica") %>% select(Sepal.Length)) # nolint
  func <- quote(select(iris %>% filter(Species == "Virginica"), Sepal.Length))

  expect_equal(unpipe(pipe), func)
})

test_that("unpipe() recognizes . syntax", {
  pipe2 <- quote(iris %>% lm(Sepal.Length ~ Sepal.Width, data = .))
  func2 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))

  expect_equal(unpipe(pipe2), func2)
})


test_that("unpipe() does not alter unpiped code", {
  func2 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))

  expect_equal(unpipe(func2), func2)
})

test_that("unpipe() can deal with missing parenthesis", {
  pipe2 <- quote(1 %>% print)
  func2 <- quote(print(1))
  expect_equal(unpipe(pipe2), func2)
  
  

})

test_that("unpipe_all() can deal with missing parenthesis", {

  pipe3 <- quote(iris %>% head %>% print)
  func3 <- quote(print(head(iris)))
  expect_equal(unpipe_all(pipe3), func3)
  
})

test_that("unpipe_all() can deal with function definitions", {
  function1 <- quote(function(x, y) x + y)
  function2 <- quote(add <- function(x, y) x + y)
  
  expect_equal(unpipe_all(function1), function1)
  expect_equal(unpipe_all(function2), function2)
})

test_that("unpipe_all() accepts code as strings", {
  pipe_str <- "a %>% b(x = 1) %>% c(y = 2)"
  pipe_quo <- quote(a %>% b(x = 1) %>% c(y = 2))
  unpipe_quo <- quote(c(b(a, x = 1), y = 2))
  
  expect_equal(unpipe_all(pipe_str), unpipe_all(pipe_quo))
  expect_equal(unpipe_all_str(pipe_str), unpipe_all_str(pipe_quo))
  expect_equal(unpipe_all_str(pipe_str), rlang::quo_text(unpipe_quo))
  
  # unpipe_all can receive strings with multiple expressions
  pipe_str2 <- "d %>% e(x = 1) %>% f(y = 2)"
  pipe_quo2 <- quote(d %>% e(x = 1) %>% f(y = 2))
  unpipe_quo2 <- quote(f(e(d, x = 1), y = 2))
  
  expect_equal(
    unpipe_all(paste0(pipe_str, "\n", pipe_str2)),
    purrr::map(list(pipe_quo, pipe_quo2), unpipe_all)
  )
  expect_equal(
    unpipe_all_str(paste0(pipe_str, "\n", pipe_str2), collapse = NULL),
    purrr::map_chr(list(pipe_quo, pipe_quo2), unpipe_all_str)
  )
  expect_equal(
    unpipe_all_str(paste0(pipe_str, "\n", pipe_str2)),
    paste(c(rlang::quo_text(unpipe_quo), rlang::quo_text(unpipe_quo2)), collapse = "\n")
  )
})
