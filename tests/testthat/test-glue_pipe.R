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
