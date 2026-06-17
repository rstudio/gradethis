# Mock a user submission to an exercise

This function helps you test your
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
and
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
logic by helping you quickly create the environment that these functions
expect when used to grade a user submission to an exercise in a learnr
tutorial.

## Usage

``` r
mock_this_exercise(
  .user_code,
  .solution_code = NULL,
  ...,
  .label = "mock",
  .engine = "r",
  .stage = "check",
  .result = rlang::missing_arg(),
  setup_global = NULL,
  setup_exercise = NULL
)
```

## Arguments

- .user_code:

  A single string or expression in braces representing the user
  submission to this exercise.

- .solution_code:

  An optional single string or expression in braces representing the
  solution code to this exercise.

- ...:

  Ignored

- .label:

  The label of the mock exercise, defaults to `"mock"`.

- .engine:

  The engine of the mock exercise. If the engine is not `"r"`, then
  `.result` must be provided explicitly since `mock_this_exercise()`
  cannot evaluate the `.user_code`.

- .stage:

  The stage of the exercise evaluation, defaults to `"check"`. learnr
  stages are `"code_check"`, `"check"` or `"error_check"`. When
  gradethis is used outside of learnr, this variable is typically
  `NULL`.

- .result:

  The result of the evaluation of the `.user_code`. If the `.engine` is
  `"r"`, the result will be prepared automatically by evaluating the
  user code.

- setup_global:

  An optional single string or expression in braces representing the
  global `setup` chunk code.

- setup_exercise:

  An optional single string or expression in braces representing the
  code in the exercise's setup chunk(s).

## Value

Returns the checking environment that is expected by
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
and
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md).
Both of these functions themselves return a function that gets called on
the checking environment. In other words, the object returned by this
function can be passed to the function returned from either
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
or
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
to test the grading logic used in either.

## Examples

``` r
# First we'll create a grading function with grade_this(). The user's code
# should return the value 42, and we have some specific messages if they're
# close but miss this target. Otherwise, we'll fall back to the default fail
# message, which will include code feedback.
this_grader <-
  grade_this({
    pass_if_equal(42, "Great Work!")
    fail_if_equal(41, "You were so close!")
    fail_if_equal(43, "Oops, just missed!")
    fail()
  })

# Our first mock submission is almost right...
this_grader(mock_this_exercise(.user_code = 41, .solution_code = 42))
#> <gradethis_graded: [Incorrect] You were so close!>

# Our second mock submission is a little too high...
this_grader(mock_this_exercise(.user_code = 43, .solution_code = 42))
#> <gradethis_graded: [Incorrect] Oops, just missed!>

# A third submission takes an unusual path, but arrives at the right answer.
# Notice that you can use braces around an expression.
this_grader(
  mock_this_exercise(
    .user_code = {
      x <- 31
      y <- 11
      x + y
    },
    .solution_code = 42
  )
)
#> <gradethis_graded: [Correct] Great Work!>

# Our final submission changes the prompt slightly. Suppose we have provided
# an `x` object in our global setup with a value of 31. We also have a `y`
# object that we create for the user in the exercise setup chunk. We then ask
# the student to add `x` and `y`. What happens if the student subtracts
# instead? That's what this mock submission tests:
this_grader(
  mock_this_exercise(
    .user_code = x - y,
    .solution_code = x + y,
    setup_global = x <- 31,
    setup_exercise = y <- 11
  )
)
#> <gradethis_graded: [Incorrect]
#>   Incorrect. I expected you to call `+` where you called `-`.
#>   Don't give up now, try it one more time.
#> >
```
