# An error checking function for use with learnr

learnr uses the checking code in `exercise.error.check.code` when the
user's submission produces an error during evaluation.
`gradethis_error_checker()` provides default error checking suitable for
most situations where an error was *not expected*.

If a solution for the exercise is available, the user's submission will
be compared to the example solution and the message to the student will
include code feedback. Otherwise, the error message from R is returned.

If you *are expecting* the user to submit code that throws an error, use
the `*-error-check` chunk to write custom grading code that validates
that the correct error was created.

## Usage

``` r
gradethis_error_checker(
  ...,
  hint = getOption("gradethis.fail.hint", TRUE),
  message = getOption("gradethis.error_checker.message", NULL),
  encourage = getOption("gradethis.fail.encourage", FALSE)
)
```

## Arguments

- ...:

  Ignored but included for future compatibility.

- hint:

  Include a code feedback hint with the failing message? This argument
  only applies to
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  and
  [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  and the message is added using the default options of
  [`give_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  and
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md).
  The default value of `hint` can be set using
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  or the `gradethis.fail.hint` option.

- message:

  The feedback message when an error occurred and no solution is
  provided for the exercise. May reference `.error` or any of the
  [grade_this-objects](https://rstudio.github.io/gradethis/reference/grade_this-objects.md).
  The default value is set by
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).

- encourage:

  Include a random encouraging phrase with
  [`random_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md)?
  The default value of `encourage` can be set using
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  or the `gradethis.fail.encourage` option.

## Value

A checking function compatible with
[`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md).

## See also

[`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md),
[`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md)

## Examples

``` r
# The default error checker is run on an exercise that produces an error.
# In the following example, the object `b` is not defined.

# This is the error that the user's submission creates:
tryCatch(
  b,
  error = function(e) message(e$message)
)
#> object 'b' not found

# If you haven't provided a model solution:
gradethis_error_checker()(mock_this_exercise(b))
#> <gradethis_graded: [Incorrect]
#>   An error occurred with your code:
#> 
#>   ```
#>   object 'b' not found
#>   ```
#> 
#> >

# If a model solution is available:
gradethis_error_checker()(mock_this_exercise(b, a))
#> <gradethis_graded: [Incorrect]
#>   An error occurred with your code:
#> 
#>   ```
#>   object 'b' not found
#>   ```
#> 
#> >
```
