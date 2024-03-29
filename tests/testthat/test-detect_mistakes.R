local_edition(3)

a <<- function(x) x
b <<- function(x) x

test_that("detect_mistakes detects surplus code", {
  # function
  user <-     quote(a(b(1)))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = solution)
  )

  user <-     quote(b(b(1)))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(
      submitted = user[[2]], solution = solution[[2]], enclosing_call = user
    )
  )

  user <-     quote(a(b(1)))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(
      submitted = user[[2]], solution = solution[[2]], enclosing_call = user
    )
  )

  # non-function
  user <-     quote(1(a(1))) # nolint
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = solution)
  )

  # internal atomic
  # arguments
  user <-     quote(b(1))
  solution <- quote(b())
  expect_equal(
    detect_mistakes(user, solution),
    message_surplus_argument(submitted_call = user, submitted = user[[2]])
  )

  # internal non-function
  user <-     quote(a(1(1))) # nolint
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(
      submitted = user[[2]], solution = solution[[2]], enclosing_call = user
    )
  )
})

test_that("detect_mistakes detects missing code", {

  # function
  user <-     quote(b(1))
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = solution)
  )


  # non-function
  user <-     quote(1(1)) # nolint
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = solution)
  )

  # internal atomic - NEEDS TO CATCH UNNAMED ARGUMENT HANDLING
  user <-     quote(a())
  solution <- quote(a(1))

  expect_equal(
    detect_mistakes(user, solution),
    message_missing_argument(submitted_call = user, solution_name = "x")
  )

  # internal function
  user <-     quote(a(1))
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = quote(1), solution = quote(b()), enclosing_call = user)
  )

  # internal non-function would not appear in a solution

})

test_that("detect_mistakes detects mis-matched code", {

  # function
  user <-     quote(b(1))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = solution)
  )

  # non-function
  user <-     quote(1(1)) # nolint
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = solution)
  )

  # internal atomic
  user <-     quote(a(1))
  solution <- quote(a(2))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = quote(1), solution = quote(2), enclosing_call = user)
  )

  # internal function
  user <-     quote(a(b(1)))
  solution <- quote(a(c(1)))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user[[2]], solution = solution[[2]], enclosing_call = user)
  )

  # internal non-function
  user <-     quote(a(1(1))) # nolint
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user[[2]], solution = solution[[2]], enclosing_call = user)
  )

})

test_that("detect_mistakes works with atomic solutions", {

  user <-     quote(2)
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = as.name("2"), solution = quote(1))
  )

  # function
  user <-     quote(a(1))
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user, solution = solution)
  )

  user <-     quote(a())
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = as.name("a()"), solution = quote(1))
  )

  user <-     quote(a(1))
  solution <- quote(pi)
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user, solution = solution)
  )

  # non-function
  user <-     quote(pi(1))
  solution <- quote(pi)
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user, solution = solution)
  )

  # internal atomics, functions, non-functions, infixes,
  # and pipes will not matter if the above tests pass.
  # Why? Because checking will stop at the initial call
  # because it is not an atomic.

})

# nolint start: comment_code
test_that("detect_mistakes works with infix operators", {

  user <- quote(123)
  solution <- quote(x <- sample(1:6, size = 1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user, solution = solution)
  )

  # changing direction of assign should still work
  user <- quote(123)
  solution <- quote(sample(1:6, size = 1) -> x)
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user, solution = solution)
  )

  # other variants of assign like <<- should also work

  user <- quote(123)
  solution <- quote(x <<- sample(1:6, size = 1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user, solution = solution)
  )

  user <- quote(123)
  solution <- quote(sample(1:6, size = 1) ->> x)
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user, solution = solution)
  )

  # call vs assign
  user <- quote(sample(1:6, size = 1))
  solution <- quote(x <- sample(1:6, size = 1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = solution)
  )

  # other operators
  expect_snapshot(detect_mistakes(quote(TRUE & FALSE), quote(TRUE | FALSE)))
  expect_snapshot(detect_mistakes(quote(2^2), quote(2*2)))
  expect_snapshot(detect_mistakes(quote(obj$value), quote(obj@value)))
  expect_snapshot(detect_mistakes(quote(y <- m * x + b), quote(y ~ m * x + b)))
  expect_snapshot(detect_mistakes(quote(1-4), quote(1:4)))
  expect_snapshot(detect_mistakes(quote(a %like% b), quote(a %LIKE% b)))

  #   # surplus
  #   user <-     quote(b(1 + 2))
  #   solution <- quote(b(1))
  #   expect_equal(
  #                detect_mistakes(user, solution)
  #                ,
  #                message_wrong_value("1 + 2", quote(1))
  #                )
  #
  #   # missing
  #   user <-     quote(sqrt(1))
  #   solution <- quote(sqrt(1 + 2))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = quote(1), that = "1 + 2")
  #   )
  #
  #   user <-     quote(sqrt(1))
  #   solution <- quote(sqrt(1 + 2 + 3))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = quote(1), that = "1 + 2 + 3")
  #   )
  #
  #   user <-     quote(sqrt(1 + 2))
  #   solution <- quote(sqrt(1 + 2 + 3))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "+ 2", that = "+ 3")
  #   )
  #
  #   user <-     quote(sqrt(1 + 3))
  #   solution <- quote(sqrt(1 + 2 + 3))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "1", that = "1 + 2")
  #   )
  #
  #   # internal infix
  #   user <-     quote(a(1 + 2))
  #   solution <- quote(a(1 + 3))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "+ 2", that = "+ 3")
  #   )
  #
  #   user <-     quote(a(1 + 2 + 4))
  #   solution <- quote(a(1 + 3 + 4))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "+ 2", that = "+ 3")
  #   )
  #
  #   user <-     quote(a(1 + 2 + 4))
  #   solution <- quote(a(1 + 3 + 5))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "+ 4", that = "+ 5")
  #   )
  #
  #   user <-     quote(a(2 + 1))
  #   solution <- quote(a(3 + 1))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "2", that = "3")
  #   )
  #
  #   user <-     quote(a(1 + 1))
  #   solution <- quote(a(1 - 1))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "1 + 1", that = "1 - 1")
  #   )
  #
  #   user <-     quote(a(1 + 1 + 1))
  #   solution <- quote(a(1 - 1 + 1))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "1 + 1", that = "1 - 1")
  #   )
  #
  #   # surplus
  #   user <-     quote(1 + 2)
  #   solution <- quote(1)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value("1 + 2", quote(1))
  #   )
  #
  #   # missing
  #   user <-     quote(1)
  #   solution <- quote(1 + 2)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = quote(1), that = "1 + 2")
  #   )
  #
  #   user <-     quote(1)
  #   solution <- quote(1 + 2 + 3)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = quote(1), that = "1 + 2 + 3")
  #   )
  #
  #   user <-     quote(1 + 2)
  #   solution <- quote(1 + 2 + 3)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "+ 2", that = "+ 3")
  #   )
  #
  #   user <-     quote(1 + 3)
  #   solution <- quote(1 + 2 + 3)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "1", that = "1 + 2")
  #   )
  #
  #   # internal infix
  #   user <-     quote(1 + 2)
  #   solution <- quote(1 + 3)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "+ 2", that = "+ 3")
  #   )
  #
  #   user <-     quote(1 + 2 + 4)
  #   solution <- quote(1 + 3 + 4)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "+ 2", that = "+ 3")
  #   )
  #
  #   user <-     quote(1 + 2 + 4)
  #   solution <- quote(1 + 3 + 5)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "+ 4", that = "+ 5")
  #   )
  #
  #   user <-     quote(2 + 1)
  #   solution <- quote(3 + 1)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "2", that = "3")
  #   )
  #
  #   user <-     quote(1 + 1)
  #   solution <- quote(1 - 1)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "1 + 1", that = "1 - 1")
  #   )
  #
  #   user <-     quote(1 + 1 + 1)
  #   solution <- quote(1 - 1 + 1)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "1 + 1", that = "1 - 1")
  #   )
  #
  #   # function
  #   user <-     quote(a(1))
  #   solution <- quote(1 + pi)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "a(1)", that = "1 + pi")
  #   )
  #
  #   user <-     quote(b(1))
  #   solution <- quote(b(1) + 2)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "b(1)", that = "b(1) + 2")
  #   )
  #
  #   user <-     quote(b(1))
  #   solution <- quote(b(1) + a(2))
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "b(1)", that = "b(1) + a(2)")
  #   )
  #
  #   # non-function
  #   user <-     quote(pi(1))
  #   solution <- quote(1 + pi)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "pi(1)", that = "1 + pi")
  #   )
  #
  #   user <-     quote(1(1)) # nolint
  #   solution <- quote(b(1) + 2)
  #   expect_equal(
  #     detect_mistakes(user, solution)
  #     ,
  #     message_wrong_value(this = "1(1)", that = "b(1) + 2")
  #   )
  #
  #   # internal atomics, functions, non-functions, infixes,
  #   # and pipes will not matter if the above tests pass.
  #   # Why? Because checking will stop at the initial call
  #   # because it is not an infix.
  #
})

test_that("detect_mistakes works with pipes", {

  # internal pipe
  user <-     quote(b(1 %>% abs()))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user[[2]], solution = solution[[2]], enclosing_call = user)
    # "In `b(1 %>% abs())`, I expected `1` where you wrote `1 %>% abs()`."
  )

  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 %>% log()))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user[[2]], solution = solution[[2]][[3]], enclosing_call = user)
  )

  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 %>% log() %>% abs()))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user[[2]], solution = solution[[2]][[3]], enclosing_call = user)
  )

  user <-     quote(sqrt(1 %>% log()))
  solution <- quote(sqrt(1 %>% log() %>% abs()))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user[[2]][[3]], solution = solution[[2]][[3]], enclosing_call = user)
  )

  ## TODO infix operator
  # user <-     quote(sqrt(1 + 2))
  # solution <- quote(sqrt(1 + 2 %>% log()))
  # expect_equal(
  #   detect_mistakes(user, solution)
  #   ,
  #   message_wrong_value(this = "+ 2", that = "+ log()")
  # )

  # internal pipe
  user <-     quote(a(2 %>% abs()))
  solution <- quote(a(2 %>% log()))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user[[2]][[3]], solution = solution[[2]][[3]], enclosing_call = user)
  )

  # DOES MESSAGE AUTOMATICALLY UNPIPE INNER ARGUMENTS?
  # user <-     quote(a(2 %>% abs() %>% sqrt()))
  # solution <- quote(a(2 %>% log() %>% sqrt()))
  # expect_equal(
  #   detect_mistakes(user, solution),
  #   message_wrong_call(submitted = user[[2]][[2]][[3]], solution = solution[[2]][[2]][[3]], enclosing_call = user[[2]])
  # )

  # TODO infix operator
  # user <-     quote(a(2 %>% abs()))
  # solution <- quote(a(2 + log(1)))
  # expect_equal(
  #   detect_mistakes(user, solution)
  #   ,
  #   message_wrong_value(this = "abs(2)", that = "2 + log(1)")
  # )

  # exernal pipe
  user <-     quote(1 %>% abs())
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user, solution = solution)
  )

  user <-     quote(1)
  solution <- quote(1 %>% log())
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = quote(1), solution = quote(log()))
  )

  user <-     quote(1)
  solution <- quote(1 %>% log() %>% abs())
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = quote(1), solution = quote(abs()))
  )

  user <-     quote(1 %>% log())
  solution <- quote(1 %>% log() %>% abs())
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user[[3]], solution = quote(abs()))
  )

  ## TODO infix operator
  # user <-     quote(1 + 2)
  # solution <- quote(1 + 2 %>% log())
  # expect_equal(
  #   detect_mistakes(user, solution)
  #   ,
  #   message_wrong_value(this = "+ 2", that = "+ log()")
  # )

  # internal pipe
  user <-     quote(2 %>% abs())
  solution <- quote(2 %>% log())
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = unpipe(user), solution = as.name("log()"))
  )

  user <-     quote(2 %>% abs() %>% sqrt())
  solution <- quote(2 %>% log() %>% sqrt())
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(
      submitted = unpipe(unpipe(user)[[2]]),
      solution = unpipe(unpipe(solution)[[2]]),
      enclosing_call = user
    )
  )

  ## TODO need to look into infix operators
  # user <-     quote(2 %>% abs())
  # solution <- quote(2 + log(1))
  # expect_equal(
  #   detect_mistakes(user, solution)
  #   ,
  #   # message_wrong_value(this = "abs(2)", that = "2 + log(1)")
  #   message_missing_argument(this_call = quote(`+`()), that = quote(log()))
  # )

  user <-     quote(b(1))
  solution <- quote(b(1) %>% a())
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = as.name("a()"))
  )

})

test_that("detect_mistakes handles a mix of named and unnamed arguments and with pipes", {
  env = new.env()
  env$fn <- function(.data, ...) .data

  expect_equal(
    detect_mistakes(
      quote(x %>% fn(name == "John")),
      quote(x %>% fn(name == "Paul")),
      user_env = env,
      solution_env = env
    ),
    message_wrong_value("John", "Paul", enclosing_call = quote(name == "John"))
  )

  expect_equal(
    detect_mistakes(
      quote(fn(x, name == "John")),
      quote(fn(.data = x, name == "Paul")),
      user_env = env,
      solution_env = env
    ),
    message_wrong_value("John", "Paul", enclosing_call = quote(name == "John"))
  )

  expect_equal(
    detect_mistakes(
      quote(fn(x = 1, 2)),
      quote(fn(x = 1)),
      user_env = env,
      solution_env = env
    ),
    message_surplus_argument(quote(fn()), quote(2), "")
  )

  expect_equal(
    detect_mistakes(
      quote(fn(x = 1, 2)),
      quote(fn(x = 1)),
      user_env = env,
      solution_env = env
    ),
    message_surplus_argument(quote(fn()), quote(2), "")
  )
})

test_that("detect_mistakes handles argument names correctly", {
  user <-     quote(c(x = a(b(1))))
  solution <- quote(c(x = b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(
      submitted = user[[2]],
      submitted_name = names(as.list(user))[2],
      solution = solution[[2]],
      enclosing_call = user
    )
  )

  user <-     quote(b(x = 1))
  solution <- quote(b(1))
  expect_null(
    detect_mistakes(user, solution)
  )

  user <-     quote(b(1))
  solution <- quote(b(x = 1))
  expect_null(
    detect_mistakes(user, solution)
  )

  user <-     quote(b(y = 1))
  solution <- quote(b(x = 1))
  expect_equal(
    detect_mistakes(user, solution),
    message_surplus_argument(
      submitted_call = quote(b()),
      submitted = quote(1),
      submitted_name = "y"
    )
  )

  user <-     quote(b(y = a(1)))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_surplus_argument(
      submitted_call = quote(b()),
      submitted = as.name("a()"),
      submitted_name = "y"
    )
  )

  test_fn <<- function(x, y = 1, z = FALSE, ...) {return(1)}

  user <-     quote(test_fn(1:10, a = 1, z = TRUE))
  solution <- quote(test_fn(1:10, b = 1, z = TRUE))
  expect_equal(
    detect_mistakes(user, solution),
    # message_wrong_value(this = quote(1),
    #             this_name = "cut",
    #             that = quote(1),
    #             that_name = "trim")
    message_surplus_argument(
      submitted_call = quote(test_fn()),
      submitted = quote(1),
      submitted_name = "a"
    )
  )

  user <-     quote(test_fn(1:10, a = 1, z = TRUE))
  solution <- quote(test_fn(1:10, 1, z = TRUE))
  expect_equal(
    detect_mistakes(user, solution),
    message_surplus_argument(
      submitted_call = quote(test_fn()),
      submitted = quote(1),
      submitted_name = "a"
    )
  )

  # This user code looks correct (and runs!) but invalid is an argument passed to
  # ... that does not appear in the solution, and so should be flagged wrong.
  user <-     quote(mean(1:10, cut = 1, invalid = TRUE))
  solution <- quote(mean(1:10, cut = 1))
  expect_equal(
    detect_mistakes(user, solution),
    message_surplus_argument(
      submitted_call = quote(mean()),
      submitted = quote(TRUE),
      submitted_name = "invalid"
    )
  )

})

test_that("detect_mistakes handles weird cases", {

  user <-     quote(sum(sum(1, 2), 3))
  solution <- quote(sum(1, 2, 3))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user[[2]], solution = solution[[2]], enclosing_call = user)
  )

  user <-     quote(sum(1, 2))
  solution <- quote(sum(1, 2, 3))
  expect_equal(
    detect_mistakes(user, solution),
    message_missing_argument(
      submitted_call = quote(sum()),
      solution_name = quote(3)
    )
  )

})


test_that("detect_mistakes checks the call first", {

  user <-     quote(0 + sqrt(log(2)))
  solution <- quote(sqrt(log(2)))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_call(submitted = user, solution = solution)
  )

})

test_that("detect_mistakes does not throw error for unused argument", {

  a <- function(x) x
  user <-     quote(a(1, y = 2))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_surplus_argument(
      submitted_call = quote(a()),
      submitted = quote(2),
      submitted_name = "y"
    )
  )

})

test_that("detect_mistakes does not throw error for multiple matches of argument", {

  z <<- function(x, ya = 1, yb = 2) x
  user <-     quote(z(1, y = 2))
  solution <- quote(z(1, ya = 2))
  expect_equal(
    detect_mistakes(user, solution),
    message_bad_argument_name(
      submitted_call = user,
      submitted = user[[3]],
      submitted_name = names(as.list(user)[3])
    )
  )

})

test_that("detect_mistakes does not throw error for multiple matches of formal", {

  zz <<- function(x, yab = 1, ...) x
  user <-     quote(zz(1, y = 2, ya = 3))
  solution <- quote(zz(1))
  expect_equal(
    detect_mistakes(user, solution),
    message_too_many_matches(submitted_call = user, solution_name = "yab")
  )

})

test_that("detect_mistakes handles duplicated argument names", {

  dd <<- function(a) a
  user <-     quote(dd(a = 1, a = 2))
  solution <- quote(dd(a = 1))
  expect_equal(
    detect_mistakes(user, solution),
    message_duplicate_name(submitted_call = user, submitted_name = "a")
  )

})

test_that("detect_mistakes does not return correct prematurely", {

  j <<- function(...) 1
  user <- quote(j(x = a(1), y = a(2)))
  user <- quote(j(x = a(x = 1), y = a(2)))
  solution <- quote(j(x = a(x = 1), y = a(3)))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = user[[3]][[2]], solution = solution[[3]][[2]], enclosing_call = user[[3]])
  )

})




test_that("detect_mistakes works with multiple lines", {

  user <- rlang::as_quosure(parse(text = "1\n2\n3\n4"), new.env())
  solution <- rlang::as_quosure(parse(text = "1\n2\n3\n4"), new.env())
  expect_null(
    detect_mistakes(user, solution)
  )

  user <- rlang::as_quosure(parse(text = "1\n8\n3\n4", new.env()))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = quote(8), solution = quote(2))
  )

  user <- rlang::as_quosure(parse(text = "1\n8\n3\n4", new.env()))
  expect_equal(
    detect_mistakes(user, solution),
    message_wrong_value(submitted = quote(8), solution = quote(2))
  )

  user <- rlang::as_quosure(parse(text = "1\n2\n3\n4\n5", new.env()))
  expect_equal(
    detect_mistakes(user, solution),
    message_extra_answer(quote(5))
  )

  user <- rlang::as_quosure(parse(text = "1\n2\n3", new.env()))
  expect_equal(
    detect_mistakes(user, solution),
    message_missing_answer(quote(3))
  )

})








test_that("detect_mistakes : Differentiate between 'strings' and objects in messages", {

  as_name <- "library(purrr)"
  as_chr <- "library('purrr')"

  expect_grade_code(
    user_code = as_name,
    solution_code = as_chr,
    is_correct = FALSE,
    msg = "In `library(purrr)`, I expected `\"purrr\"` where you wrote `purrr`."
  )
  expect_grade_code(
    user_code = as_chr,
    solution_code = as_name,
    is_correct = FALSE,
    msg = "In `library(\"purrr\")`, I expected `purrr` where you wrote `\"purrr\"`."
  )

})


test_that("detect_mistakes works with function arguments", {
  expect_match(
    detect_mistakes(
      as.pairlist(alist(x = , y = )),
      as.pairlist(alist(x = , y = , z = ))
    ),
    "I expected argument `z`"
  )

  expect_grade_code(
    user_code = "function(x, y, z) x + y",
    solution_code = "function(x, y) x + y",
    is_correct = FALSE,
    msg = "I didn't expect argument `z` where you wrote `function(x, y, z)`."
  )

  expect_grade_code(
    user_code = "function(x, y) x + y",
    solution_code = "function(y, x) x + y",
    is_correct = FALSE,
    msg = "In `function(x, y)`, I expected arguments `y`, `x` where you wrote arguments `x`, `y`."
  )

  expect_grade_code(
    user_code = "function(x, y) x + y",
    solution_code = "function(x, y = 1) x + y",
    is_correct = FALSE,
    msg = "In `function(x, y)`, I expected arguments `x`, `y = 1` where you wrote arguments `x`, `y`."
  )

  expect_grade_code(
    user_code = "function(x = 1, y = 2) x + y",
    solution_code = "function(x, y = 1) x + y",
    is_correct = FALSE,
    msg = "In `function(x = 1, y = 2)`, I expected arguments `x`, `y = 1` where you wrote arguments `x = 1`, `y = 2`."
  )

  expect_equal(
    code_feedback("function(x, y = a1 %>% b) x + y", "function(x, y = b(a)) x + y"),
    "In `function(x, y = a1 %>% b)`, I expected arguments `x`, `y = b(a)` where you wrote arguments `x`, `y = a1 %>% b`."
  )

  expect_equal(
    code_feedback("function(x, y) y2 %>% x", "function(x, y) y %>% x"),
    "In `y2 %>% x`, I expected `y` where you wrote `y2`."
  )
})


test_that("detect_mistakes returns a reasonable amount of intro context", {
  ggplot_1 <- 'ggplot(data = penguins, mapping = aes(x = flipper_length_mm, fill = species)) +
    geom_density(alpha = 0.4) +
    labs(title = "Gentoos have the longest flippers", x = "Flipper length (mm)", y = "Density") +
    scale_color_brewer(palette = "Set1")'
  ggplot_2 <- 'ggplot(data = penguins, mapping = aes(x = flipper_length_mm, fill = species)) +
    geom_density(alpha = 0.4) +
    labs(title = "Gentoos have the longest flippers", x = "Flipper length (mm)", y = "Density") +
    scale_fill_brewer(palette = "Set1")'

  feedback <- code_feedback(ggplot_1, ggplot_2)
  expect_false(grepl("^In ", feedback))
  expect_match(feedback, "scale_color_brewer", fixed = TRUE)
  expect_match(feedback, "scale_fill_brewer", fixed = TRUE)
})

test_that("detect_mistakes says 'didn't expect' when there are too many things", {
  expect_grade_code(
    user_code = "a$b",
    solution_code = "a",
    is_correct = FALSE,
    msg = "I didn't expect `$` where you wrote `a$b`."
  )

  expect_grade_code(
    user_code = "a == b",
    solution_code = "a",
    is_correct = FALSE,
    msg = "I didn't expect `==` where you wrote `a == b`."
  )

  expect_grade_code(
    user_code = "a * b",
    solution_code = "a",
    is_correct = FALSE,
    msg = "I didn't expect `*` where you wrote `a * b`."
  )
})
# nolint end
