context("Check code with calls")
a <- function(x) x
b <- function(x) x

test_that("detect_mistakes detects surplus code", {

  # function
  user <-     quote(a(b(1)))
  solution <- quote(b(1))
  expect_equal(
               detect_mistakes(user, solution)
               ,
               wrong_value(this = "a(b(1))", that = quote(b()))
               )

  user <-     quote(b(b(1)))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "b(1)", that = quote(1))
  )

  user <-     quote(a(b(1)))
  solution <- quote(a(1))
  expect_equal(
               detect_mistakes(user, solution)
               ,
               wrong_value(this = "b(1)", that = quote(1))
               )

  # non-function
  user <-     quote(1(a(1)))
  solution <- quote(a(1))
  expect_equal(
               detect_mistakes(user, solution)
               ,
               wrong_value(this = "1(a(1))", that = "a()")
               )

  # internal atomic
  # arguments
  user <-     quote(b(1))
  solution <- quote(b())
  expect_equal(
               detect_mistakes(user, solution)
               ,
               surplus_argument(quote(b()), quote(1))
               )

  # internal non-function
  user <-     quote(a(1(1)))
  solution <- quote(a(1))
  expect_equal(
               detect_mistakes(user, solution)
               ,
               wrong_value(this = "1(1)", that = quote(1))
               )
})

test_that("detect_mistakes detects missing code", {

  # function
  user <-     quote(b(1))
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "b(1)", that = quote(a()))
  )


  # non-function
  user <-     quote(1(1))
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1(1)", that = quote(a()))
  )

  # internal atomic
  user <-     quote(a())
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    missing_argument(this_call = quote(a()), that = quote(1))
  )

  # internal function
  user <-     quote(a(1))
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = quote(b()))
  )

  # internal non-function would not appear in a solution

})

test_that("detect_mistakes detects mis-matched code", {

  # function
  user <-     quote(b(1))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "b(1)", that = quote(a()))
  )

  # non-function
  user <-     quote(1(1))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1(1)", that = quote(a()))
  )

  # internal atomic
  user <-     quote(a(1))
  solution <- quote(a(2))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = quote(2))
  )

  # internal function
  user <-     quote(a(b(1)))
  solution <- quote(a(c(1)))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "b(1)", that = quote(c()))
  )

  # internal non-function
  user <-     quote(a(1(1)))
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1(1)", that = quote(b()))
  )

})

test_that("detect_mistakes works with atomic solutions", {

  user <-     quote(2)
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "2", that = quote(1))
  )

  # function
  user <-     quote(a(1))
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "a(1)", that = quote(1))
  )

  user <-     quote(a())
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "a()", that = quote(1))
  )

  user <-     quote(a(1))
  solution <- quote(pi)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "a(1)", that = quote(pi))
  )

  # non-function
  user <-     quote(pi(1))
  solution <- quote(pi)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "pi(1)", that = quote(pi))
  )

  # internal atomics, functions, non-functions, infixes,
  # and pipes will not matter if the above tests pass.
  # Why? Because checking will stop at the initial call
  # because it is not an atomic.

})

test_that("detect_mistakes works with infix operators", {

  # surplus
  user <-     quote(b(1 + 2))
  solution <- quote(b(1))
  expect_equal(
               detect_mistakes(user, solution)
               ,
               wrong_value("1 + 2", quote(1))
               )

  # missing
  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 + 2))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = "1 + 2")
  )

  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 + 2 + 3))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = "1 + 2 + 3")
  )

  user <-     quote(sqrt(1 + 2))
  solution <- quote(sqrt(1 + 2 + 3))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 2", that = "+ 3")
  )

  user <-     quote(sqrt(1 + 3))
  solution <- quote(sqrt(1 + 2 + 3))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1", that = "1 + 2")
  )

  # internal infix
  user <-     quote(a(1 + 2))
  solution <- quote(a(1 + 3))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 2", that = "+ 3")
  )

  user <-     quote(a(1 + 2 + 4))
  solution <- quote(a(1 + 3 + 4))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 2", that = "+ 3")
  )

  user <-     quote(a(1 + 2 + 4))
  solution <- quote(a(1 + 3 + 5))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 4", that = "+ 5")
  )

  user <-     quote(a(2 + 1))
  solution <- quote(a(3 + 1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "2", that = "3")
  )

  user <-     quote(a(1 + 1))
  solution <- quote(a(1 - 1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1 + 1", that = "1 - 1")
  )

  user <-     quote(a(1 + 1 + 1))
  solution <- quote(a(1 - 1 + 1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1 + 1", that = "1 - 1")
  )

  # surplus
  user <-     quote(1 + 2)
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value("1 + 2", quote(1))
  )

  # missing
  user <-     quote(1)
  solution <- quote(1 + 2)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = "1 + 2")
  )

  user <-     quote(1)
  solution <- quote(1 + 2 + 3)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = "1 + 2 + 3")
  )

  user <-     quote(1 + 2)
  solution <- quote(1 + 2 + 3)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 2", that = "+ 3")
  )

  user <-     quote(1 + 3)
  solution <- quote(1 + 2 + 3)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1", that = "1 + 2")
  )

  # internal infix
  user <-     quote(1 + 2)
  solution <- quote(1 + 3)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 2", that = "+ 3")
  )

  user <-     quote(1 + 2 + 4)
  solution <- quote(1 + 3 + 4)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 2", that = "+ 3")
  )

  user <-     quote(1 + 2 + 4)
  solution <- quote(1 + 3 + 5)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 4", that = "+ 5")
  )

  user <-     quote(2 + 1)
  solution <- quote(3 + 1)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "2", that = "3")
  )

  user <-     quote(1 + 1)
  solution <- quote(1 - 1)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1 + 1", that = "1 - 1")
  )

  user <-     quote(1 + 1 + 1)
  solution <- quote(1 - 1 + 1)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1 + 1", that = "1 - 1")
  )

  # function
  user <-     quote(a(1))
  solution <- quote(1 + pi)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "a(1)", that = "1 + pi")
  )

  user <-     quote(b(1))
  solution <- quote(b(1) + 2)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "b(1)", that = "b(1) + 2")
  )

  user <-     quote(b(1))
  solution <- quote(b(1) + a(2))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "b(1)", that = "b(1) + a(2)")
  )

  # non-function
  user <-     quote(pi(1))
  solution <- quote(1 + pi)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "pi(1)", that = "1 + pi")
  )

  user <-     quote(1(1))
  solution <- quote(b(1) + 2)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "1(1)", that = "b(1) + 2")
  )

  # internal atomics, functions, non-functions, infixes,
  # and pipes will not matter if the above tests pass.
  # Why? Because checking will stop at the initial call
  # because it is not an infix.

})

test_that("detect_mistakes works with pipes", {

  # internal pipe
  user <-     quote(b(1 %>% abs()))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value("abs(1)", quote(1))
  )

  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 %>% log()))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = quote(log()))
  )

  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 %>% log() %>% abs()))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = quote(abs()))
  )

  user <-     quote(sqrt(1 %>% log()))
  solution <- quote(sqrt(1 %>% log() %>% abs()))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "log(1)", that = quote(abs()))
  )

  user <-     quote(sqrt(1 + 2))
  solution <- quote(sqrt(1 + 2 %>% log()))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 2", that = "+ log()")
  )

  # internal pipe
  user <-     quote(a(2 %>% abs()))
  solution <- quote(a(2 %>% log()))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "abs(2)", that = "log()")
  )

  user <-     quote(a(2 %>% abs() %>% sqrt()))
  solution <- quote(a(2 %>% log() %>% sqrt()))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "abs(2)", that = "log()")
  )

  user <-     quote(a(2 %>% abs()))
  solution <- quote(a(2 + log(1)))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "abs(2)", that = "2 + log(1)")
  )

  # exernal pipe
  user <-     quote(1 %>% abs())
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value("abs(1)", quote(1))
  )

  user <-     quote(1)
  solution <- quote(1 %>% log())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = quote(log()))
  )

  user <-     quote(1)
  solution <- quote(1 %>% log() %>% abs())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1), that = quote(abs()))
  )

  user <-     quote(1 %>% log())
  solution <- quote(1 %>% log() %>% abs())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "log(1)", that = quote(abs()))
  )

  user <-     quote(1 + 2)
  solution <- quote(1 + 2 %>% log())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "+ 2", that = "+ log()")
  )

  # internal pipe
  user <-     quote(2 %>% abs())
  solution <- quote(2 %>% log())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "abs(2)", that = "log()")
  )

  user <-     quote(2 %>% abs() %>% sqrt())
  solution <- quote(2 %>% log() %>% sqrt())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "abs(2)", that = "log()")
  )

  user <-     quote(2 %>% abs())
  solution <- quote(2 + log(1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "abs(2)", that = "2 + log(1)")
  )

  user <-     quote(b(1))
  solution <- quote(b(1) %>% a())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "b(1)", that = "a()")
  )

})

test_that("detect_mistakes handles argument names correctly", {

  user <-     quote(c(x = a(b(1))))
  solution <- quote(c(x = b(1)))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = "a(b(1))",
                that = quote(b()),
                this_name = "x",
                that_name = "x")
  )

  user <-     quote(b(x = 1))
  solution <- quote(b())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    surplus_argument(this_call =  quote(b()),
                     this = quote(1),
                     this_name = "x")
  )

  user <-     quote(b(x = a(1)))
  solution <- quote(b())
  expect_equal(
    detect_mistakes(user, solution)
    ,
    surplus_argument(this_call =  quote(b()),
                     this = "a(1)",
                     this_name = "x")
  )

  user <-     quote(mean(1:10, cut = 1, na.rm = TRUE))
  solution <- quote(mean(1:10, trim = 1, na.rm = TRUE))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1),
                this_name = "cut",
                that = quote(1),
                that_name = "trim")
  )

  user <-     quote(mean(1:10, cut = 1, na.rm = TRUE))
  solution <- quote(mean(1:10, 1, na.rm = TRUE))
  expect_null(
    detect_mistakes(user, solution)
  )

  user <-     quote(mean(1:10, cut = 1, na.rm = TRUE))
  solution <- quote(mean(1:10, TRUE, cut = 1))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this = quote(1),
                this_name = "cut",
                that = quote(TRUE))
  )

})

test_that("detect_mistakes handles weird cases", {

  user <-     quote(sum(sum(1, 2), 3))
  solution <- quote(sum(1, 2, 3))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    wrong_value(this =  "sum(1, 2)", that = quote(1))
  )

  user <-     quote(sum(1, 2))
  solution <- quote(sum(1, 2, 3))
  expect_equal(
    detect_mistakes(user, solution)
    ,
    missing_argument(this_call =  quote(sum()),
                     that = quote(3))
  )

})
