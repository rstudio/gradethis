context("glue_pipe option")

test_that("glue_pipe in grade_this_code() is equivalent to grade_code()", {
  exercise <- list2env(list(
    .user_code = "penguins %>% pull(year) %>% min(year)",
    .solution_code = "penguins %>% pull(year) %>% min()"
  ))
  
  expect_equal(
    grade_this_code(incorrect = "{.message}")(exercise)$message,
    grade_code(glue_incorrect = "{.message}")(exercise)$message
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
      msg = glue_message_pipe(gradethis_default_options$gradethis_glue_pipe, .user_code = user_code, .message = "")
    )
  )
})
