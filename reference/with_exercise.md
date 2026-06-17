# Run an expression as if it were in an exercise's `grade_this()` block

This function is not intended to be used within grading code, but may be
helpful for testing grading code.

## Usage

``` r
with_exercise(exercise, expr)
```

## Arguments

- exercise:

  An exercise, as created by
  [`mock_this_exercise()`](https://rstudio.github.io/gradethis/reference/mock_this_exercise.md)

- expr:

  An unquoted expression

## Value

The value of `grade_this(<expr>)(exercise)`

## Examples

``` r
exercise <- mock_this_exercise(.user_code = "2", .solution_code = "1 + 1")

with_exercise(exercise, pass_if_equal())
#> <gradethis_graded: [Correct] What first-rate work! Correct!>
with_exercise(exercise, fail_if_code_feedback())
#> <gradethis_graded: [Incorrect]
#>   I expected `+` where you wrote `2`.
#> >
```
