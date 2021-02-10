context("Check grade learnr")

test_that("Grade learnr grade_result", {
  expect_grade_learnr(
    user_code = "4",
    check_code = "grade_result(
      pass_if(~ .result == 4, 'yes. you did it.'),
      correct = 'this other correct message.',
      glue_correct = '{ .message } { .correct }'
    )",
    is_correct = TRUE,
    msg = "yes. you did it. this other correct message."
  )
})

test_that("Grade learnr check_code", {
  expect_grade_learnr(
    user_code = "4",
    check_code = "grade_code(correct = 'This works', glue_correct = '{.correct}')",
    solution_code = "4",
    is_correct = TRUE,
    msg = "This works"
  )

  expect_grade_learnr(
    user_code = "exp(log(2))",
    check_code = "grade_code(glue_incorrect = '{.message}')",
    solution_code = "exp(log(1))",
    is_correct = FALSE,
    msg = "In `log(2)`, I expected `1` where you wrote `2`."
  )

  # User code that produces a parsing error should return an incorrect grade (by default)
  expect_grade_learnr(
    user_code = "function(",
    check_code = 'function(...) stop("boom")',
    solution_code = "4",
    is_correct = FALSE,
    msg = I("might not be valid R code") # from default parse error in grade_learnr_parse_error()
  )
  
  # Code scaffolding produces informative parsing error message
  expect_grade_learnr(
    user_code = "____(mtcars, cyl)",
    is_correct = FALSE,
    msg = I("contains 1 blank")
  )
  
  expect_grade_learnr(
    user_code = "________(___, ____)",
    is_correct = FALSE,
    msg = I("contains 3 blanks")
  )
  
  # Can customize the feedback through an exercise.parse.error function
  parse_error_func <- function(x) {
    graded(
      correct = FALSE,
      message = paste0(
        "The user code of '", x$.user_code, "' produced a parsing error."
      )
    )
  }
  with_options(
    list(exercise.parse.error = parse_error_func),
    expect_grade_learnr(
      user_code = "function(",
      msg = "The user code of 'function(' produced a parsing error.",
      is_correct = FALSE
    )
  )
})

test_that("gradethis_setup", {
  with_options(gradethis_setup(), {
    gradethis_pass <- getOption("gradethis.pass", NULL)
    gradethis_fail <- getOption("gradethis.fail", NULL)
    gradethis_code_incorrect <- getOption("gradethis.code.incorrect", NULL)
    expect_type(gradethis_pass, "character")
    expect_type(gradethis_fail, "character")
    expect_type(gradethis_code_incorrect, "character")
    
    expect_silent(glue::glue(gradethis_pass))
    expect_silent(glue::glue(gradethis_fail))
    expect_silent(glue::glue(gradethis_code_incorrect, .message = "Foo"))
  })
})