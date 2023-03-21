user_code <- rlang::expr({
  # ```{r example}
  x <- 1
  y <- list(1, 2, 3)
  z <- function() print("Hello World!")
  # ```
})

exercise <- mock_this_exercise(!!user_code)
.envir_result <- exercise$.envir_result
.envir_prep <- exercise$.envir_prep

user_object_list()
user_object_exists("x")
user_object_get("y")

# Use `mode` to find only objects of a certain type ----
user_object_list(mode = "numeric")
user_object_list(mode = "list")
user_object_list(mode = "function")

user_object_exists("x", mode = "numeric")
user_object_exists("y", mode = "numeric")

user_object_get("z", mode = "function")

# By default, `user_object_list()` ignores objects created by setup chunks ----
setup_code <- rlang::expr({
  # ```{r example-setup}
  exercise_data <- mtcars
  # ```
})

exercise <- mock_this_exercise(!!user_code, setup_exercise = !!setup_code)
.envir_result <- exercise$.envir_result
.envir_prep <- exercise$.envir_prep

user_object_list()

## You can disable this by setting `exclude_envir = NULL` ----
user_object_list(exclude_envir = NULL)
