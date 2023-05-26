# code_feedback() ---------------------------------------------------------

test_that("code_feedback() returns a string if there are differences or NULL", {
  expect_null(code_feedback("a", "a"))
  expect_null(code_feedback("runif(1)", "runif(1)"))
  expect_null(code_feedback("a %>% b(x = 1) %>% c(y = 2)", "c(b(a, x = 1), y = 2)"))

  expect_type(code_feedback("a", "b"), "character")
  expect_type(code_feedback("runif(1)", "rnorm(1)"), "character")
  expect_type(
    code_feedback("a %>% b(x = 1) %>% c(y = 2)", "d(b(a, x = 1), y = 2)"),
    "character"
  )
})

test_that("code_feedback() finds the closest match if multiple solutions", {
  expect_equal(
    code_feedback("a", solution_code = gradethis_solutions("aa", "bb")),
    "I expected `aa` where you wrote `a`."
  )

  # If there's a tie including the last option, the last option is selected
  expect_equal(
    code_feedback("a", solution_code = gradethis_solutions("b", "c")),
    "I expected `c` where you wrote `a`."
  )

  # If there's a tie not including the last option, the first option is selected
  expect_equal(
    code_feedback("a", solution_code = gradethis_solutions("b", "c", "xyz")),
    "I expected `b` where you wrote `a`."
  )
})

test_that("code_feedback() converts solution_code to solution_code_all", {
  expect_equal(
    code_feedback("a", solution_code = gradethis_solutions("aa", "bb")),
    "I expected `aa` where you wrote `a`."
  )

  expect_equal(
    code_feedback("a", solution_code = gradethis_solutions("b", "c")),
    "I expected `c` where you wrote `a`."
  )

  expect_equal(
    code_feedback("a", solution_code = gradethis_solutions("b", "c", "xyz")),
    "I expected `b` where you wrote `a`."
  )
})

test_that("code_feedback() checks ...", {
  expect_error(code_feedback("a", "b", c = 12))
})

test_that("code_feedback() checks `env`", {
  expect_error(code_feedback("a", "b", env = "c"))
})

test_that("to_expr() accepts quosures or strings", {
  expect_equal(
    to_expr(rlang::quo(base::acos(0.42)), name = "solution"),
    to_expr("base::acos(0.42)", name = "solution")
  )

  expect_match(
    code_feedback(
      rlang::quo(base::acos(0.42)),
      "base::atan(0.42)"
    ),
    "expected.+atan.+acos"
  )

  expect_error(to_expr(42))
})

test_that("code_feedback() works with S3 methods", {
  expect_equal(
    gradethis::code_feedback(
      "mydata <- head(THIS_VARIABLE_DOES_NOT_EXIST)",
      "mydata <- head(mtcars)"
    ),
    "In `head(THIS_VARIABLE_DOES_NOT_EXIST)`, I expected `mtcars` where you wrote `THIS_VARIABLE_DOES_NOT_EXIST`."
  )
})

# maybe_code_feedback() ---------------------------------------------------

test_that("maybe_code_feedback() formals match with code_feedback()", {
  expect_true(all(
    names(formals(code_feedback)) %in% names(formals(maybe_code_feedback))
  ))
})

test_that("maybe_code_feedback() spaces", {

  expect_equal(
    expect_grade_this(
      fail("{maybe_code_feedback()}"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    " In `log(2)`, I expected `3` where you wrote `2`."
  )

  with_options(
    list(lifecycle_verbosity = "quiet"),
    expect_equal(
      expect_grade_this(
        fail("{ maybe_code_feedback(space_before = FALSE, space_after = FALSE) }"),
        "sqrt(log(2))",
        "sqrt(log(3))",
        is_correct = FALSE
      )$message,
      "In `log(2)`, I expected `3` where you wrote `2`."
    )
  )

  expect_equal(
    expect_grade_this(
      fail("{ maybe_code_feedback(before = NULL, after = NULL) }"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    "In `log(2)`, I expected `3` where you wrote `2`."
  )

  with_options(
    list(lifecycle_verbosity = "quiet"),
    expect_equal(
      expect_grade_this(
        fail("{ maybe_code_feedback(space_before = FALSE, space_after = TRUE) }"),
        "sqrt(log(2))",
        "sqrt(log(3))",
        is_correct = FALSE
      )$message,
      "In `log(2)`, I expected `3` where you wrote `2`. "
    )
  )

  expect_equal(
    expect_grade_this(
      fail("{ maybe_code_feedback(before = NULL, after = ' ') }"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    "In `log(2)`, I expected `3` where you wrote `2`. "
  )

  with_options(
    list(lifecycle_verbosity = "quiet"),
    expect_equal(
      expect_grade_this(
        fail("{ maybe_code_feedback(space_before = TRUE, space_after = 1) }"),
        "sqrt(log(2))",
        "sqrt(log(3))",
        is_correct = FALSE
      )$message,
      " In `log(2)`, I expected `3` where you wrote `2`."
    )
  )

  expect_equal(
    expect_grade_this(
      fail("{ maybe_code_feedback(before = ' ', after = NULL) }"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    " In `log(2)`, I expected `3` where you wrote `2`."
  )

  with_options(
    list(lifecycle_verbosity = "quiet"),
    expect_equal(
      expect_grade_this(
        fail("{ maybe_code_feedback(space_before = TRUE, space_after = TRUE) }"),
        "sqrt(log(2))",
        "sqrt(log(3))",
        is_correct = FALSE
      )$message,
      " In `log(2)`, I expected `3` where you wrote `2`. "
    )
  )
  expect_equal(
    expect_grade_this(
      fail("{ maybe_code_feedback(before = ' ', after = ' ') }"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    " In `log(2)`, I expected `3` where you wrote `2`. "
  )

  expect_equal(
    expect_grade_this(
      fail("{ maybe_code_feedback(before = NULL, after = c('', '')) }"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    "In `log(2)`, I expected `3` where you wrote `2`.\n"
  )

  expect_equal(
    expect_grade_this(
      fail("{ maybe_code_feedback(before = '\n', after = '\n') }"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    "\nIn `log(2)`, I expected `3` where you wrote `2`.\n"
  )

  expect_equal(
    expect_grade_this(
      fail("{ maybe_code_feedback(before = '<span class=\"cf\">', after = '</span>') }"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    "<span class=\"cf\">In `log(2)`, I expected `3` where you wrote `2`.</span>"
  )
})

test_that("maybe_code_feedback() is used in grade_this_code", {

  expect_this_code(
    "sqrt(log(2))",
    "sqrt(log(3))",
    incorrect = "A fail message.{maybe_code_feedback()}",
    is_correct = FALSE
  )

})

test_that("maybe_code_feedback() is not used when no solution is available", {

  with_gradethis_setup(
    fail = "A fail message.{maybe_code_feedback()}",
    {
      expect_equal(
        expect_grade_this(
          user_code = "2 + 2",
          {
            fail()
          },
          is_correct = FALSE
        )$message,
        "A fail message."
      )
    }
  )

  with_gradethis_setup(
    fail = "A fail message. {maybe_code_feedback(default = 'No feedback!')}",
    {
      expect_equal(
        expect_grade_this(
          user_code = "2 + 2",
          {
            fail()
          },
          is_correct = FALSE
        )$message,
        "A fail message. No feedback!"
      )
    }
  )

})

test_that("maybe_code_feedback() is used when solution is available", {

  with_gradethis_setup(
    fail = "A fail message.{maybe_code_feedback()}",
    {
      expect_equal(
        expect_grade_this(
          user_code = "sqrt(log(2))",
          solution_code = "sqrt(log(3))",
          {
            fail()
          },
          is_correct = FALSE
        )$message,
        "A fail message. In `log(2)`, I expected `3` where you wrote `2`."
      )
    }
  )

})

test_that("maybe_code_feedback() is not included when gradethis.maybe_code_feedback is FALSE", {

  with_gradethis_setup(
    fail = "A fail message.{maybe_code_feedback()}",
    maybe_code_feedback = FALSE,
    {
      expect_equal(
        expect_grade_this(
          user_code = "sqrt(log(2))",
          solution_code = "sqrt(log(3))",
          {
            fail()
          },
          is_correct = FALSE
        )$message,
        "A fail message."
      )
    }
  )

})

test_that("maybe_code_feedback() can be overwritten by local fail() message", {

  with_gradethis_setup(
    fail = "A fail message.{ maybe_code_feedback() }",
    {
      expect_equal(
        expect_grade_this(
          user_code = "sqrt(log(2))",
          solution_code = "sqrt(log(3))",
          {
            fail("custom message")
          },
          is_correct = FALSE
        )$message,
        "custom message"
      )
    }
  )

})

test_that("maybe_code_feedback() returned default message if no feedback", {
  with_gradethis_setup(
    fail = "A fail message.{maybe_code_feedback(default = ' DEFAULT')}",
    {
      expect_equal(
        expect_grade_this(
          user_code = "sqrt(log(4))",
          solution_code = "sqrt(log(4))",
          {
            fail()
          },
          is_correct = FALSE
        )$message,
        "A fail message. DEFAULT"
      )
    }
  )
})

# give_code_feedback() ----------------------------------------------------

submission_wrong <- mock_this_exercise(
  .user_code = "log(4)",
  .solution_code = "sqrt(4)"
)

test_that("give_code_feedback() adds feedback to character messages", {
  expect_equal(
    give_code_feedback("Message."),
    "Message.{maybe_code_feedback()}"
  )
  expect_equal(
    give_code_feedback("Message.", location = "after"),
    "Message.{maybe_code_feedback()}"
  )
  expect_equal(
    give_code_feedback("Message.", location = "before"),
    "{maybe_code_feedback()}Message."
  )
})

test_that("give_code_feedback() checks basic assumptions", {
  expect_error(give_code_feedback(location = "between"))
  expect_error(give_code_feedback(12))
  expect_error(give_code_feedback(FALSE))
})

test_that("give_code_feedback() wraps grade_this()", {
  expect_match(
    expect_exercise_checker(
      user_code = "log(4)",
      solution_code = "sqrt(4)",
      check_code = 'give_code_feedback(grade_this({
       pass_if_equal(.solution, "Good job!")
       if (.result < 2) {
         fail("Too low!")
       }
       fail()
     }))',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "I expected.+sqrt.+log"
  )
  expect_false(
    grepl(
      "I expected",
      expect_exercise_checker(
        user_code = "sqrt(4)",
        solution_code = "sqrt(4)",
        check_code = 'give_code_feedback(grade_this({
       pass_if_equal(.solution, "Good job!")
       if (.result < 2) {
         fail("Too low!")
       }
       fail()
     }))',
        is_correct = TRUE,
        msg = NULL
      )$message
    )
  )
})

test_that("give_code_feedback() wraps grades, does not affect passing grades", {
  expect_match(
    expect_exercise_checker(
      user_code = "log(4)",
      solution_code = "sqrt(4)",
      check_code = 'grade_this({
       pass_if_equal(.solution, "Good job!")
       if (.result < 2) {
         give_code_feedback(fail("Too low!"))
       }
       fail()
     })',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "I expected.+sqrt.+log"
  )

  expect_false(
    grepl(
      "I expected",
      expect_exercise_checker(
        user_code = "sqrt(4)",
        solution_code = "sqrt(4)",
        check_code = 'grade_this({
       give_code_feedback(pass_if_equal(.solution, "Good job!"))
       if (.result < 2) {
         give_code_feedback(fail("Too low!"))
       }
       fail()
     })',
        is_correct = TRUE,
        msg = NULL
      )$message
    )
  )
})

test_that("give_code_feedback() catches testthat errors", {
  expect_match(
    expect_exercise_checker(
      user_code = "5",
      solution_code = "5L",
      check_code = 'grade_this({
       give_code_feedback(fail_if_error(testthat::expect_type(.result, "integer")))
       pass()
     })',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "\\.result.+has type.+I expected"
  )

  expect_match(
    expect_exercise_checker(
      user_code = "5",
      solution_code = "5L",
      check_code = 'give_code_feedback(grade_this({
       fail_if_error(testthat::expect_type(.result, "integer"))
       pass()
     }))',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "\\.result.+has type.+I expected"
  )
})

test_that("give_code_feedback() catches plain errors", {
  expect_match(
    expect_exercise_checker(
      user_code = "apple",
      solution_code = "banana",
      check_code = 'grade_this({
       give_code_feedback(fail_if_error(stop("nope;")))
       pass()
     })',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "nope; I expected"
  )

  expect_match(
    expect_exercise_checker(
      user_code = "apple",
      solution_code = "banana",
      check_code = 'give_code_feedback(grade_this({
       fail_if_error(stop("nope;"))
       pass()
     }))',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "nope; I expected"
  )
})

test_that("give_code_feedback() doesn't add feedback twice", {
  str_count <- function(string, pattern) {
    stopifnot(length(string) == 1)
    m <- gregexpr(pattern, string)[[1]]
    if (m[1] < 0) return(0)
    length(m)
  }

  feedback <- expect_exercise_checker(
    user_code = "apple",
    solution_code = "banana",
    check_code = 'grade_this({
       give_code_feedback(fail("{maybe_code_feedback()}"))
     })',
    is_correct = FALSE,
    msg = NULL
  )$message

  expect_equal(str_count(feedback, "I expected"), 1)

  feedback <-
    with_gradethis_setup(
      fail = "{maybe_code_feedback()}",
      maybe_code_feedback = TRUE,
      expect_exercise_checker(
        user_code = "apple",
        solution_code = "banana",
        check_code = "grade_this({
       give_code_feedback(fail())
     })",
        is_correct = FALSE,
        msg = NULL
      )$message
    )

  expect_equal(str_count(feedback, "I expected"), 1)

  ## Multiple Solutions ----
  feedback <- expect_exercise_checker(
    user_code = "sum(1:10)",
    solution_code = "
# mean ----
mean(1:10)
# median ----
median(1:10)",
    check_code = 'grade_this({
       fail("{maybe_code_feedback()}", hint = TRUE)
     })',
    is_correct = FALSE,
    msg = NULL
  )$message

  expect_equal(str_count(feedback, "I expected"), 1)
})

test_that("give_code_feedback() works with pipes", {
  expect_match(
    expect_exercise_checker(
      user_code = "log(4)",
      solution_code = "sqrt(4)",
      check_code = 'grade_this({
       pass_if_equal(.solution, "Good job!")
       if (.result < 2) {
         fail("Too low!")
       }
       fail()
     }) %>% give_code_feedback()',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "I expected.+sqrt.+log"
  )

  expect_match(
    expect_exercise_checker(
      user_code = "log(4)",
      solution_code = "sqrt(4)",
      check_code = 'grade_this({
       pass_if_equal(.solution, "Good job!")
       if (.result < 2) {
         fail("Too low!") %>% give_code_feedback()
       }
       fail()
     })',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "I expected.+sqrt.+log"
  )

  expect_match(
    expect_exercise_checker(
      user_code = "apple",
      solution_code = "banana",
      check_code = 'grade_this({
       fail_if_error(stop("nope;")) %>% give_code_feedback()
       pass()
     })',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "nope; I expected"
  )

  expect_match(
    expect_exercise_checker(
      user_code = "apple",
      solution_code = "banana",
      check_code = 'grade_this({
       fail_if_error(stop("nope;"))
       pass()
     }) %>% give_code_feedback()',
      is_correct = FALSE,
      msg = NULL
    )$message,
    "nope; I expected"
  )
})


test_that("give_code_feedback() does nothing for NULL", {
  expect_match(
    expect_exercise_checker(
      user_code = "apple",
      solution_code = "banana",
      check_code = 'grade_this({
       give_code_feedback(if (FALSE) stop("nope;"))
       pass("okay")
     }) %>% give_code_feedback()',
      is_correct = TRUE,
      msg = NULL
    )$message,
    "okay"
  )
})


# fail() with hints -------------------------------------------------------

test_that("fail('msg', hint = TRUE) gives code feedback with custom message", {
  grader <- quote(
    grade_this({
      if (.result == 1) fail("Nothing special.")
      if (.result == 2) fail("Not two!", hint = TRUE)
      fail_if_equal(3, "Not three!", hint = TRUE)
      fail_if(.result == 5, "Not five!", hint = TRUE)
      fail(hint = TRUE)
      pass("TEST FAILED")
    })
  )

  expect_exercise_checker(
    user_code = "1",
    solution_code = "4",
    check_code = deparse_to_string(grader),
    is_correct = FALSE,
    msg = "Nothing special."
  )

  expect_match(
    expect_exercise_checker(
      user_code = "2",
      solution_code = "4",
      check_code = deparse_to_string(grader),
      is_correct = FALSE,
      msg = NULL
    )$message,
    "Not two! I expected"
  )

  expect_match(
    expect_exercise_checker(
      user_code = "3",
      solution_code = "4",
      check_code = deparse_to_string(grader),
      is_correct = FALSE,
      msg = NULL
    )$message,
    "Not three! I expected"
  )

  expect_match(
    expect_exercise_checker(
      user_code = "5",
      solution_code = "4",
      check_code = deparse_to_string(grader),
      is_correct = FALSE,
      msg = NULL
    )$message,
    "Not five! I expected"
  )

  expect_false(
    grepl(
      "I expected",
      expect_exercise_checker(
        user_code = "4",
        solution_code = "4",
        check_code = deparse_to_string(grader),
        is_correct = FALSE,
        msg = NULL
      )$message
    )
  )
})

test_that("fail() message doesn't duplicate hints", {
  ex <- mock_this_exercise(.user_code = 2, .solution_code = 1)
  ex_feedback <- code_feedback("2", "1")

  expect_fail_message <- function(expr, global_hint_true, global_hint_false = global_hint_true) {
    expr <- rlang::enquo(expr)
    msg <- list(
      global_hint_true = with_options(
        list(
          gradethis.fail.hint = TRUE,
          gradethis.fail = "PREFACE.{maybe_code_feedback()} CONCLUSION"
        ),
        rlang::eval_tidy(expr)$message
      ),
      global_hint_false = with_options(
        list(
          gradethis.fail.hint = FALSE,
          gradethis.fail = "PREFACE.{maybe_code_feedback()} CONCLUSION"
        ),
        rlang::eval_tidy(expr)$message
      )
    )
    msg <- lapply(msg, as.character)

    global_hint_true <- as.character(glue::glue(global_hint_true, .envir = parent.frame()))
    global_hint_false <- as.character(glue::glue(global_hint_false, .envir = parent.frame()))

    expect_equal(!!msg$global_hint_true, !!global_hint_true)
    expect_equal(!!msg$global_hint_false, !!global_hint_false)
  }

  # default message, fall back on global fail.hint option
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail()
    })(ex),
    "PREFACE. {ex_feedback} CONCLUSION"
  )

  # default message but hint = FALSE locally
  # => hint = FALSE wins
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail(hint = FALSE)
    })(ex),
    global_hint_true = "PREFACE. CONCLUSION"
  )

  # default fail message, but hint = TRUE locally
  # => hint is added by default message
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail(hint = TRUE)
    })(ex),
    global_hint_true = "PREFACE. {ex_feedback} CONCLUSION"
  )

  # custom fail message with feedback, inherit hint
  # => template wins in both
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail("Nice try, but{maybe_code_feedback()}")
    })(ex),
    global_hint_true = "Nice try, but {ex_feedback}"
  )

  # custom fail message with feedback, with local hint = FALSE
  # => template wins in all
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail("Nice try, but{maybe_code_feedback()}", hint = FALSE)
    })(ex),
    global_hint_true = "Nice try, but {ex_feedback}"
  )

  # custom fail message with feedback, with local hint = TRUE
  # => template wins in all
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail("Nice try, but{maybe_code_feedback()}", hint = TRUE)
    })(ex),
    global_hint_true = "Nice try, but {ex_feedback}"
  )

  # custom fail message without feedback, inherit hint
  # => global option decides
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail("Nice try!")
    })(ex),
    global_hint_true = "Nice try! {ex_feedback}",
    global_hint_false = "Nice try!"
  )

  # custom fail message without feedback, with local hint = FALSE
  # => local option overrides global option
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail("Nice try!", hint = FALSE)
    })(ex),
    global_hint_true = "Nice try!"
  )

  # custom fail message without feedback, with local hint = TRUE
  # => local option overrides global option
  expect_fail_message(
    grade_this({
      pass_if_equal()
      fail("Nice try!", hint = TRUE)
    })(ex),
    global_hint_true = "Nice try! {ex_feedback}"
  )
})
