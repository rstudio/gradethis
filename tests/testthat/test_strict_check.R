context("Strict check")

# these tests are largely redundant exercises that have been tested against detect_mistakes()
expect_correct <- function(x) {
  expect_s3_class(x, "grader_result")
  expect_true(x$correct)
}
expect_message <- function(x, message) {
  expect_s3_class(x, "grader_result")
  expect_true(!x$correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Spots differences in atomics", {

  user <- quote(1)
  solution <- quote(1)
  expect_correct(
    check_code(user = user, solution = solution)
  )

  user <- quote(1)
  solution <- quote(2)
  expect_message(
    check_code(user = user, solution = solution),
    wrong_value(this = quote(1), that = quote(2))
  )
})

test_that("Spots differences in names", {

  user <- quote(x)
  solution <- quote(y)
  expect_message(
    check_code(user = user, solution = solution),
    wrong_value(this = quote(x), that = quote(y))
  )

  user <- quote(x)
  solution <- quote(x)
  expect_correct(
    check_code(user = user, solution = solution)
  )

  user <- quote(5)
  solution <- quote(y)
  expect_message(
    check_code(user = user, solution = solution),
    wrong_value(this = quote(5), that = quote(y))
  )
})

test_that("Spots differences in calls", {
  a <- quote(vapply(lists, mean, numeric(1), na.rm = TRUE))
  b <- quote(vapply(vecs, mean, numeric(1), na.rm = TRUE))
  c <- quote(vapply(lists, mean, numeric(1)))
  d <- quote(vapply(vecs, mean, numeric(1)))

  expect_correct(
    check_code(user = a, solution = a)
  )

  expect_message(
    check_code(user = a, solution = b),
    wrong_value(this = quote(lists), that = quote(vecs))
  )

  expect_message(
    check_code(user = a, solution = c),
    surplus_argument(this_call = "vapply()",
                     this_name = "na.rm",
                     this = quote(TRUE))
  )

  expect_message(
    check_code(user = c, solution = a),
    missing_argument(this_call = "vapply()",
                     that_name = "na.rm",
                     that = quote(TRUE))
  )
})

test_that("Mentions only first non-matching element", {
  w <- quote(1)
  x <- quote(log(1))
  y <- quote(sqrt(log(2)))
  z <- quote(sqrt(log(1)))

  expect_correct(
    check_code(user = w, solution = w)
  )

  expect_message(
    check_code(user = w, solution = z),
    wrong_value(this = quote(1), that = quote(sqrt()))
  )

  expect_message(
    check_code(user = x, solution = z),
    wrong_value(this = "log(1)", that = quote(sqrt()))
  )

  expect_message(
    check_code(user = y, solution = z),
    wrong_value(this = "2", that = quote(1))
  )

})

test_that("Spots differences in argument names", {
  a <- quote(mean(1:10, trim = 1, na.rm = TRUE))
  b <- quote(mean(1:10, 1, TRUE))
  c <- quote(mean(1:10, cut = 1, na.rm = TRUE))

  expect_correct(
    check_code(user = a, solution = a)
  )

  expect_correct(
    check_code(user = b, solution = a)
  )

  expect_message(
    check_code(user = c, solution = a),
    wrong_value(this = quote(1), this_name = "cut",
                that = quote(1), that_name = "trim")
  )

})

test_that("Ignore differences in argument positions (for non ... arguments)", {
  a <- quote(round(x = pi, digits = 2))
  b <- quote(round(pi, digits = 2))
  c <- quote(round(2, x = pi))
  d <- quote(round(digits = 2, x = pi))

  expect_correct(
    check_code(user = b, solution = a)
  )

  expect_correct(
    check_code(user = c, solution = a)
  )

  expect_correct(
    check_code(user = d, solution = a)
  )

  expect_correct(
    check_code(user = a, solution = d)
  )

})

test_that("Returns intelligent error when no solution code", {

  expect_error(
    check_code(),
    "No solution is provided for this exercise."
  )

})

test_that("Returns intelligent error when no user code", {
  expect_error(
    check_code(solution = quote(5)),
    "I didn't receive your code. Did you write any?"
  )
})

test_that("Spot differences when pipes are involved", {

  select <- function(df, x) {
    rlang::eval_tidy(rlang::quos(x), df)
  }
  filter <- subset
  arrange <- function(df, ...) {
    df
  }

  pipe <- quote(iris %>% filter(Species == "Virginica") %>% select(Sepal.Length))
  func <- quote(select(iris %>% filter(Species == "Virginica"), Sepal.Length))
  func1 <- quote(select(filter(iris, Species == "Virginica"), Sepal.Length))
  pipe1 <- quote(iris %>% filter(Species == "Virginica") %>% select(Petal.Length))
  pipe2 <- quote(iris %>% arrange(Species) %>% select(Sepal.Length))
  pipe3 <- quote(iris %>% lm(Sepal.Length ~ Sepal.Width, data = .))
  func3 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))

  expect_correct(check_code(user = func,  solution = pipe))
  expect_correct(check_code(user = func1, solution = pipe))
  expect_correct(check_code(user = pipe,  solution = func))
  expect_correct(check_code(user = pipe,  solution = func1))
  expect_correct(check_code(user = pipe,  solution = pipe))
  expect_correct(check_code(user = func,  solution = func1))
  expect_correct(check_code(user = func1, solution = func1))
  expect_correct(check_code(user = func3, solution = pipe3))
  expect_correct(check_code(user = pipe3, solution = func3))
  expect_correct(check_code(user = pipe3, solution = pipe3))

})
