
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

# maybe_code_feedback() ---------------------------------------------------

test_that("formals match with code_feedback()", {
  expect_true(all(
    names(formals(code_feedback)) %in% names(formals(maybe_code_feedback))
  ))
})


test_that("spaces", {
  
  expect_equal(
    expect_grade_this(
      fail("{maybe_code_feedback()}"),
      "sqrt(log(2))",
      "sqrt(log(3))",
      is_correct = FALSE
    )$message,
    " In `log(2)`, I expected `3` where you wrote `2`."
  )
  lifecycle::expect_deprecated(
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
  lifecycle::expect_deprecated(
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
  lifecycle::expect_deprecated(
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
  lifecycle::expect_deprecated(
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

test_that("is used in grade_this_code", {
  
  expect_this_code(
    "sqrt(log(2))",
    "sqrt(log(3))",
    incorrect = "A fail message.{maybe_code_feedback()}",
    is_correct = FALSE
  )
  
})


test_that("is not used when no solution is available", {
  
  with_options(
    list(gradethis.fail = "A fail message.{maybe_code_feedback()}"),
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
  
  with_options(
    list(gradethis.fail = "A fail message. {maybe_code_feedback(default = 'No feedback!')}"),
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

test_that("is used when solution is available", {
  
  with_options(
    list(gradethis.fail = "A fail message.{maybe_code_feedback()}"),
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

test_that("is not included when gradethis.maybe_code_feedback is FALSE", {

  with_options(
    list(
      gradethis.fail = "A fail message.{maybe_code_feedback()}",
      gradethis.maybe_code_feedback = FALSE
    ),
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


test_that("can be overwritten by local fail() message", {
  
  with_options(
    list(gradethis.fail = "A fail message.{ maybe_code_feedback() }"),
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


# with_code_feedback() ----------------------------------------------------

submission_wrong <- mock_this_exercise(
  .user_code = "log(4)", 
  .solution_code = "sqrt(4)"
)

test_that("with_code_feedback() adds feedback to character messages", {
  expect_equal(
    with_code_feedback("Message."),
    "Message.{maybe_code_feedback()}"
  )
  expect_equal(
    with_code_feedback("Message.", location = "after"),
    "Message.{maybe_code_feedback()}"
  )
  expect_equal(
    with_code_feedback("Message.", location = "before"),
    "{maybe_code_feedback()}Message."
  )
})

test_that("with_code_feedback() checks basic assumptions", {
  expect_error(with_code_feedback(location = "between"))
  expect_error(with_code_feedback(12))
  expect_error(with_code_feedback(FALSE))
})

test_that("with_code_feedback() wraps grade_this()", {
  expect_match(
    expect_exercise_checker(
      user_code = "log(4)",
      solution_code = "sqrt(4)",
      check_code = 'with_code_feedback(grade_this({
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
        check_code = 'with_code_feedback(grade_this({
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

test_that("with_code_feedback() wraps grades, does not affect passing grades", {
  expect_match(
    expect_exercise_checker(
      user_code = "log(4)",
      solution_code = "sqrt(4)",
      check_code = 'grade_this({
       pass_if_equal(.solution, "Good job!")
       if (.result < 2) {
         with_code_feedback(fail("Too low!"))
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
       with_code_feedback(pass_if_equal(.solution, "Good job!"))
       if (.result < 2) {
         with_code_feedback(fail("Too low!"))
       }
       fail()
     })',
        is_correct = FALSE,
        msg = NULL
      )$message
    )
  )
})
