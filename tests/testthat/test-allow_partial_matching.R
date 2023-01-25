
test_that("grade_code() - allow_partial_matching works 2 errors", {
  expect_grade_code(
    user_code = "purrr::insistently(mean,quie = TRUE,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,quiet = TRUE,rate = rate_backoff())",
    is_correct = TRUE
  )
  expect_grade_code(
    user_code = "purrr::insistently(mean,quie = TRUE,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,quiet = TRUE,rate = rate_backoff())",
    allow_partial_matching = TRUE,
    is_correct = TRUE
  )
  expect_grade_code(
    user_code = "purrr::insistently(mean,quie = TRUE,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,quiet = TRUE,rate = rate_backoff())",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "purrr::insistently",
      submitted = list(quote(TRUE), quote(rate_backoff())),
      submitted_name = c("quie", "rat"),
      solution_name = c("quiet", "rate")
    )
  )
})

test_that("grade_this_code() - allow_partial_matching works 2 errors", {

  expect_this_code(
    user_code = "purrr::insistently(mean,quie = TRUE,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,quiet = TRUE,rate = rate_backoff())",
    is_correct = TRUE
  )
  expect_this_code(
    user_code = "purrr::insistently(mean,quie = TRUE,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,quiet = TRUE,rate = rate_backoff())",
    allow_partial_matching = TRUE,
    is_correct = TRUE
  )
  expect_this_code(
    user_code = "purrr::insistently(mean,quie = TRUE,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,quiet = TRUE,rate = rate_backoff())",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "purrr::insistently",
      submitted = list(quote(TRUE), quote(rate_backoff())),
      submitted_name = c("quie", "rat"),
      solution_name = c("quiet", "rate")
    )
  )
})

test_that("grade_code() - allow_partial_matching works 1 error bool", {

  expect_grade_code(
    user_code = "purrr::insistently(mean,quie = TRUE)",
    solution_code = "purrr::insistently(mean,quiet = TRUE)",
    is_correct = TRUE
  )
  expect_grade_code(
    user_code = "purrr::insistently(mean,quie = TRUE)",
    solution_code = "purrr::insistently(mean,quiet = TRUE)",
    allow_partial_matching = TRUE,
    is_correct = TRUE
  )
  expect_grade_code(
    user_code = "purrr::insistently(mean,quie = TRUE)",
    solution_code = "purrr::insistently(mean,quiet = TRUE)",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "purrr::insistently",
      submitted = quote(TRUE),
      submitted_name = c("quie"),
      solution_name = c("quiet")
    )
  )
})

test_that("grade_this_code() - allow_partial_matching works 1 error bool", {

  expect_this_code(
    user_code = "purrr::insistently(mean,quie = TRUE)",
    solution_code = "purrr::insistently(mean,quiet = TRUE)",
    is_correct = TRUE
  )
  expect_this_code(
    user_code = "purrr::insistently(mean,quie = TRUE)",
    solution_code = "purrr::insistently(mean,quiet = TRUE)",
    allow_partial_matching = TRUE,
    is_correct = TRUE
  )
  expect_this_code(
    user_code = "purrr::insistently(mean,quie = TRUE)",
    solution_code = "purrr::insistently(mean,quiet = TRUE)",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "purrr::insistently",
      submitted = quote(TRUE),
      submitted_name = c("quie"),
      solution_name = c("quiet")
    )
  )
})

test_that("grade_code() - allow_partial_matching works 1 error fun", {

  expect_grade_code(
    user_code = "purrr::insistently(mean,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,rate = rate_backoff())",
    is_correct = TRUE
  )
  expect_grade_code(
    user_code = "purrr::insistently(mean,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,rate = rate_backoff())",
    allow_partial_matching = TRUE,
    is_correct = TRUE
  )
  expect_grade_code(
    user_code = "purrr::insistently(mean,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,rate = rate_backoff())",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "purrr::insistently",
      submitted = list(quote(rate_backoff())),
      submitted_name = "rat",
      solution_name = "rate"
    )
  )
})

test_that("grade_this_code() - allow_partial_matching works 1 error fun", {

  expect_this_code(
    user_code = "purrr::insistently(mean,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,rate = rate_backoff())",
    is_correct = TRUE
  )
  expect_this_code(
    user_code = "purrr::insistently(mean,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,rate = rate_backoff())",
    allow_partial_matching = TRUE,
    is_correct = TRUE
  )
  expect_this_code(
    user_code = "purrr::insistently(mean,rat = rate_backoff())",
    solution_code = "purrr::insistently(mean,rate = rate_backoff())",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "purrr::insistently",
      submitted = list(quote(rate_backoff())),
      submitted_name = "rat",
      solution_name = "rate"
    )
  )
})

test_that("grade_code() - allow_partial_matching works 1 error chr", {

  ff <- function(p1 = "yes") print(p1)

  expect_grade_code(
    user_code = "ff(p=\"no\")",
    solution_code = "ff(p1=\"no\")",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "ff",
      submitted = quote("no"),
      submitted_name = "p",
      solution_name = "p1"
    )
  )
})

test_that("grade_this_code() - allow_partial_matching works 1 error chr", {

  ff <- function(p1 = "yes") print(p1)

  expect_this_code(
    user_code = "ff(p=\"no\")",
    solution_code = "ff(p1=\"no\")",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "ff",
      submitted = quote("no"),
      submitted_name = "p",
      solution_name = "p1"
    )
  )
})

test_that("grade_code() - allow_partial_matching works errors multi arg type", {

  ff <- function(chr = "yes", fun = ls, call = ls(), bool = TRUE) {
    print("youpi")
  }

  pmatch_FALSE <- expect_grade_code( # nolint: object_name
    user_code = "ff(ch = \"yes\", fu = ls, cal =ls(), boo = TRUE)",
    solution_code = "ff(chr = \"yes\", fun = ls, call =ls(), bool = TRUE)",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "ff",
      submitted = list(quote("yes"), quote(ls), quote(ls()), quote(TRUE)),
      submitted_name = c("ch", "fu", "cal", "boo"),
      solution_name = c("chr", "fun", "call", "bool")
    )
  )
  expect_match(object = pmatch_FALSE$message, regexp = "ch = \"yes\"")
  expect_match(object = pmatch_FALSE$message, regexp = "chr = \"yes\"")
  expect_match(object = pmatch_FALSE$message, regexp = "boo = TRUE")
  expect_match(object = pmatch_FALSE$message, regexp = "bool = TRUE")
  expect_match(object = pmatch_FALSE$message, regexp = "cal = ls()")
  expect_match(object = pmatch_FALSE$message, regexp = "call = ls()")
  expect_match(object = pmatch_FALSE$message, regexp = "fun = ls")
  expect_match(object = pmatch_FALSE$message, regexp = "fu = ls")


})

test_that("grade_this_code() - allow_partial_matching works errors multi arg type", {

  ff <- function(chr = "yes", fun = ls, call = ls(), bool = TRUE) {
    print("youpi")
  }

  pmatch_FALSE <- expect_this_code( # nolint: object_name
    user_code = "ff(ch = \"yes\", fu = ls, cal =ls(), boo = TRUE)",
    solution_code = "ff(chr = \"yes\", fun = ls, call =ls(), bool = TRUE)",
    allow_partial_matching = FALSE,
    is_correct = FALSE,
    msg = message_pmatches_argument_name(
      submitted_call = "ff",
      submitted = list(quote("yes"), quote(ls), quote(ls()), quote(TRUE)),
      submitted_name = c("ch", "fu", "cal", "boo"),
      solution_name = c("chr", "fun", "call", "bool")
    )
  )
  expect_match(object = pmatch_FALSE$message, regexp = "ch = \"yes\"")
  expect_match(object = pmatch_FALSE$message, regexp = "chr = \"yes\"")
  expect_match(object = pmatch_FALSE$message, regexp = "boo = TRUE")
  expect_match(object = pmatch_FALSE$message, regexp = "bool = TRUE")
  expect_match(object = pmatch_FALSE$message, regexp = "cal = ls()")
  expect_match(object = pmatch_FALSE$message, regexp = "call = ls()")
  expect_match(object = pmatch_FALSE$message, regexp = "fun = ls")
  expect_match(object = pmatch_FALSE$message, regexp = "fu = ls")


})

test_that("grade_code() - allow_partial_matching works with multiple matches", {

  ff <-  function(ab, abc, abcd) return(1)

  expect_grade_code(
    user_code = "ff(ab = 1)",
    solution_code = "ff(ab = 1)",
    allow_partial_matching = FALSE,
    is_correct = TRUE
  )
  expect_grade_code(
    user_code = "ff(a = 1)",
    solution_code = "ff(a = 1)",
    allow_partial_matching = FALSE,
    is_correct = TRUE
  )
  expect_equal(
    expect_grade_code(
      user_code = "ff(abc = 1)",
      solution_code = "ff(ab = 1, abc = 1)",
      glue_correct = "{ .message } { .correct }",
      glue_incorrect = "{ .message } { .incorrect }",
      allow_partial_matching = FALSE,
      is_correct = FALSE
    ),
    expect_grade_code(
      user_code = "ff(abc = 1)",
      solution_code = "ff(ab = 1, abc = 1)",
      glue_correct = "{ .message } { .correct }",
      glue_incorrect = "{ .message } { .incorrect }",
      allow_partial_matching = TRUE,
      is_correct = FALSE
    )
  )

  expect_grade_code(
    user_code = "ff(ab = 1, abc = 1)",
    solution_code = "ff(ab = 1, abc = 1)",
    is_correct = TRUE
  )
  expect_grade_code(
    user_code = "ff(abc = 1, ab = 1)",
    solution_code = "ff(ab = 1, abc = 1)",
    is_correct = TRUE
  )

})

test_that("grade_this_code() - allow_partial_matching works with multiple matches", {

  ff <-  function(ab, abc, abcd) return(1)

  expect_this_code(
    user_code = "ff(ab = 1)",
    solution_code = "ff(ab = 1)",
    allow_partial_matching = FALSE,
    is_correct = TRUE
  )
  expect_this_code(
    user_code = "ff(a = 1)",
    solution_code = "ff(a = 1)",
    allow_partial_matching = FALSE,
    is_correct = TRUE
  )
  expect_equal(
    expect_this_code(
      user_code = "ff(abc = 1)",
      solution_code = "ff(ab = 1, abc = 1)",
      correct = "correct",
      incorrect = "{.message}",
      allow_partial_matching = FALSE,
      is_correct = FALSE
    ),
    expect_this_code(
      user_code = "ff(abc = 1)",
      solution_code = "ff(ab = 1, abc = 1)",
      correct = "correct",
      incorrect = "{.message}",
      allow_partial_matching = TRUE,
      is_correct = FALSE
    )
  )

  expect_this_code(
    user_code = "ff(ab = 1, abc = 1)",
    solution_code = "ff(ab = 1, abc = 1)",
    is_correct = TRUE
  )
  expect_this_code(
    user_code = "ff(abc = 1, ab = 1)",
    solution_code = "ff(ab = 1, abc = 1)",
    is_correct = TRUE
  )

})
