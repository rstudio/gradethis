context("glue_pipe option")

test_that("glue_pipe in grade_this_code() is equivalent to grade_code()", {
  user_code <-  "penguins %>% pull(year) %>% min(year)"
  solution_code <-  "penguins %>% pull(year) %>% min()"
  
  expect_equal(
    expect_this_code(
      user_code = user_code,
      solution_code = solution_code,
      incorrect = "{.message_pipe_warning}{.message}",
      is_correct = FALSE,
      msg = "I see that you are using pipe"
    )$message,
    expect_grade_code(
      user_code = user_code,
      solution_code = solution_code,
      glue_incorrect = "{.message_pipe_warning}{.message}",
      is_correct = FALSE,
      msg = "I see that you are using pipe"
    )$message
  )
})

test_that("glue_pipe message returns unpiped text", {
  user_code <- "x %>% a() %>% b() %>% c()"
  expect_grade_code(
    user_code = user_code,
    solution_code = "b(a(x))",
    is_correct = FALSE,
    glue_pipe = "{.user_code_unpiped}",
    msg = unpipe_all_str(user_code)
  )
  
  with_options(
    list(gradethis.glue_pipe = gradethis_default_options$gradethis_glue_pipe),
    expect_grade_code(
      user_code = user_code,
      solution_code = "b(a(x))",
      is_correct = FALSE,
      msg = glue_pipe_message(gradethis_default_options$gradethis_glue_pipe, .user_code = user_code)
    )
  )
})

test_that("glue_pipe can be disabled by setting equal to NULL", {
  user_code = "x %>% b()"
  solution_code = "x %>% a()"
  
  with_options(
    list(gradethis_glue_pipe = NULL),
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

test_that("glue_pipe can be disabled by setting equal to empty character string", {
  user_code = "x %>% b()"
  solution_code = "x %>% a()"
  
  with_options(
    list(gradethis_glue_pipe = ""),
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
