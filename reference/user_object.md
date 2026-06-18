# Functions for interacting with objects created by student and solution code

Functions for interacting with objects created by student and solution
code

## Usage

``` r
user_object_get(x, mode = "any", ..., check_env = parent.frame())

solution_object_get(x, mode = "any", ..., check_env = parent.frame())

user_object_exists(x, mode = "any", ..., check_env = parent.frame())

solution_object_exists(x, mode = "any", ..., check_env = parent.frame())

user_object_list(
  mode = "any",
  exclude_envir = .envir_prep,
  ...,
  check_env = parent.frame()
)

solution_object_list(
  mode = "any",
  exclude_envir = .envir_prep,
  ...,
  check_env = parent.frame()
)
```

## Arguments

- x:

  An object name, given as a quoted
  [character](https://rdrr.io/r/base/character.html) string.

- mode:

  character specifying the [`mode`](https://rdrr.io/r/base/mode.html) of
  objects to consider. Passed to
  [`exists`](https://rdrr.io/r/base/exists.html) and
  [`get`](https://rdrr.io/r/base/get.html).

- exclude_envir:

  An [environment](https://rdrr.io/r/base/environment.html). Objects
  that appear in `exclude_envir` will be excluded from results. Defaults
  to
  [`.envir_prep`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md).
  Use `exclude_envir = NULL` to include all objects.

- ...:

  Additional arguments passed to underlying functions:

  - For `user_object_exists()` and `solution_object_exists()`,
    [`exists()`](https://rdrr.io/r/base/exists.html)

  - For `user_object_get()` and `solution_object_get()`,
    [`get()`](https://rdrr.io/r/base/get.html)

  - For `user_object_list()` and `solution_object_list()`,
    [`ls.str()`](https://rdrr.io/r/utils/ls_str.html)

- check_env:

  The [environment](https://rdrr.io/r/base/environment.html) from which
  to retrieve
  [`.envir_result`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  and
  [`.envir_prep`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md).
  Most users of gradethis will not need to use this argument.

## Value

For `user_object_get()` and `solution_object_get()`, the object. If the
object is not found, an error.

For `user_object_exists()` and `solution_object_exists()`, a
[`TRUE`](https://rdrr.io/r/base/logical.html)/[`FALSE`](https://rdrr.io/r/base/logical.html)
value.

For `user_object_list()` and `solution_object_list()`, a
[character](https://rdrr.io/r/base/character.html) vector giving the
names of the objects created by the student or solution code.

## Examples

``` r
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
#> [1] "x" "y" "z"
with_exercise(exercise, user_object_exists("x"))
#> [1] TRUE
with_exercise(exercise, user_object_get("x"))
#> [1] "I'm student code!"

with_exercise(exercise, solution_object_list())
#> [1] "x" "y" "z"
with_exercise(exercise, solution_object_exists("x"))
#> [1] TRUE
with_exercise(exercise, solution_object_get("x"))
#> [1] "I'm solution code!"

# Use `mode` to find only objects of a certain type ----

with_exercise(exercise, user_object_list(mode = "character"))
#> [1] "x"
with_exercise(exercise, user_object_list(mode = "list"))
#> [1] "y"
with_exercise(exercise, user_object_list(mode = "function"))
#> [1] "z"

with_exercise(exercise, user_object_exists("x", mode = "character"))
#> [1] TRUE
with_exercise(exercise, user_object_exists("y", mode = "character"))
#> [1] FALSE

with_exercise(exercise, user_object_get("z", mode = "function"))
#> function () 
#> print("Hello World!")
#> <environment: 0x5602358e05c0>

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
#> [1] "x" "y" "z"

## You can disable this by setting `exclude_envir = NULL` ----

with_exercise(setup_exercise, user_object_list(exclude_envir = NULL))
#> [1] "setup_data" "x"          "y"          "z"         
```
