
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
    incorrect = "A fail message.{ maybe_code_feedback() }",
    is_correct = FALSE
  )

})


test_that("is not used when no solution is available", {

  with_options(
    list(gradethis.fail = "A fail message.{ maybe_code_feedback()}"),
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

})

test_that("is used when solution is available", {

  with_options(
    list(gradethis.fail = "A fail message.{ maybe_code_feedback() }"),
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


test_that("can be overwritten", {

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
