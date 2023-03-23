user_code <- quote({
  # ```{r example}
  x <- "I'm student code!"
  y <- list(1, 2, 3)
  z <- function() print("Hello World!")
  # ```
})

solution_code <- quote({
  # ```{r example-solution}
  x <- "I'm solution code!"
  y <- list("a", "b", "c")
  z <- function() print("Goodnight Moon!")
  # ```
})

exercise <- mock_this_exercise(!!user_code, !!solution_code)

with_exercise(exercise, user_object_list())
with_exercise(exercise, user_object_exists("x"))
with_exercise(exercise, user_object_get("x"))

with_exercise(exercise, solution_object_list())
with_exercise(exercise, solution_object_exists("x"))
with_exercise(exercise, solution_object_get("x"))

# Use `mode` to find only objects of a certain type ----

with_exercise(exercise, user_object_list(mode = "character"))
with_exercise(exercise, user_object_list(mode = "list"))
with_exercise(exercise, user_object_list(mode = "function"))

with_exercise(exercise, user_object_exists("x", mode = "character"))
with_exercise(exercise, user_object_exists("y", mode = "character"))

with_exercise(exercise, user_object_get("z", mode = "function"))

# By default, `user_object_list()` ignores objects created by setup chunks ----

setup_code <- rlang::expr({
  # ```{r example-setup}
  setup_data <- mtcars
  # ```
})

setup_exercise <- mock_this_exercise(
  !!user_code, !!solution_code, setup_exercise = !!setup_code
)

with_exercise(setup_exercise, user_object_list())

## You can disable this by setting `exclude_envir = NULL` ----

with_exercise(setup_exercise, user_object_list(exclude_envir = NULL))
