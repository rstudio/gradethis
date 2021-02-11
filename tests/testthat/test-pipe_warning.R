context("pipe_warning()")

test_that("pipe_warning() in grade_this_code() is equivalent to grade_code()", {
  user_code <-  "penguins %>% pull(year) %>% min(year)"
  solution_code <-  "penguins %>% pull(year) %>% min()"
  
  expect_equal(
    expect_this_code(
      user_code = user_code,
      solution_code = solution_code,
      incorrect = "{pipe_warning()}{.message}",
      is_correct = FALSE,
      msg = "I see that you are using pipe"
    )$message,
    expect_grade_code(
      user_code = user_code,
      solution_code = solution_code,
      glue_incorrect = "{pipe_warning()}{.message}",
      is_correct = FALSE,
      msg = "I see that you are using pipe"
    )$message
  )
})

test_that("pipe_warning() message returns unpiped text", {
  user_code <- "x %>% a() %>% b() %>% c()"
  with_options(
    list(gradethis.pipe_warning = "{.user_code_unpiped}"),
    expect_grade_code(
      user_code = user_code,
      solution_code = "b(a(x))",
      is_correct = FALSE,
      msg = unpipe_all_str(user_code)
    )
  )
  
  with_options(
    list(gradethis.glue_pipe = gradethis_default_options$gradethis.pipe_warning),
    expect_grade_code(
      user_code = user_code,
      solution_code = "b(a(x))",
      is_correct = FALSE,
      msg = pipe_warning(gradethis_default_options$gradethis.pipe_warning, .user_code = user_code)
    )
  )
})

test_that("pipe_warning() can be disabled by setting equal to NULL", {
  user_code = "x %>% b()"
  solution_code = "x %>% a()"
  
  with_options(
    list(gradethis.pipe_warning = NULL),
    {
      expect_equal(
        expect_grade_code(
          user_code = user_code,
          solution_code = solution_code,
          glue_incorrect = "INCORRECT",
          is_correct = FALSE,
          msg = NULL
        )$message,
        "INCORRECT"
      )
      
      expect_equal(
        expect_this_code(
          user_code = user_code,
          solution_code = solution_code,
          incorrect = "INCORRECT",
          is_correct = FALSE,
          msg = NULL
        )$message,
        "INCORRECT"
      )
    }
  )
})

test_that("pipe_warning() can be disabled by setting equal to empty character string", {
  user_code = "x %>% b()"
  solution_code = "x %>% a()"
  
  with_options(
    list(gradethis.pipe_warning = ""),
    {
      expect_equal(
        expect_grade_code(
          user_code = user_code,
          solution_code = solution_code,
          glue_incorrect = "INCORRECT",
          is_correct = FALSE,
          msg = NULL
        )$message,
        "INCORRECT"
      )
      
      expect_equal(
        expect_this_code(
          user_code = user_code,
          solution_code = solution_code,
          incorrect = "INCORRECT",
          is_correct = FALSE,
          msg = NULL
        )$message,
        "INCORRECT"
      )
    }
  )
})

test_that("glue_pipe is deprecated in grade_code()", {
  user_code <- "x %>% a() %>% b() %>% c()"
  expect_warning(
    expect_grade_code(
      user_code = user_code,
      solution_code = "b(a(x))",
      is_correct = FALSE,
      glue_pipe = "{.user_code_unpiped}",
      msg = unpipe_all_str(user_code)
    )
  )
})
