# Signal a failing grade if mistakes are detected in the submitted code

`fail_if_code_feedback()` uses
[`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
to detect if there are differences between the user's submitted code and
the solution code (if available). If the exercise does not have an
associated solution, or if there are no detected differences between the
user's and the solution code, no grade is returned.

See
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
for more information on gradethis grade-signaling functions.

## Usage

``` r
fail_if_code_feedback(
  message = NULL,
  user_code = .user_code,
  solution_code = .solution_code_all,
  ...,
  env = parent.frame(),
  hint = TRUE,
  encourage = getOption("gradethis.fail.encourage", FALSE),
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE)
)
```

## Arguments

- message:

  A character string of the message to be displayed. In all grading
  helper functions other than
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md),
  `message` is a template string that will be processed with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- user_code, solution_code:

  String containing user or solution code. By default, when used in
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
  [.user_code](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  is retrieved for the
  [.user_code](https://rstudio.github.io/gradethis/reference/grade_this-objects.md).
  `solution_code` may also be a list containing multiple solution
  variations, so by default in
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  [.solution_code_all](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  is found and used for `solution_code`. You may also use
  `.solution_code` if there is only one solution.

- ...:

  Arguments passed on to
  [`graded`](https://rstudio.github.io/gradethis/reference/graded.md)

  `correct`

  :   A logical value of whether or not the checked code is correct.

  `type,location`

  :   The `type` and `location` of the feedback object provided to
      learnr. See
      <https://rstudio.github.io/learnr/exercises.html#Custom_checking>
      for more details.

      `type` may be one of "auto", "success", "info", "warning",
      "error", or "custom".

      `location` may be one of "append", "prepend", or "replace".

  `praise`

  :   Include a random praising phrase with
      [`random_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)?
      The default value of `praise` can be set using
      [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
      or the `gradethis.pass.praise` option.

- env:

  Environment used to standardize formals of the user and solution code.
  Defaults to retrieving
  [.envir_result](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  and
  [.envir_solution](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  from [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html).

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

- encourage:

  Include a random encouraging phrase with
  [`random_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md)?
  The default value of `encourage` can be set using
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  or the `gradethis.fail.encourage` option.

- allow_partial_matching:

  A logical. If `FALSE`, the partial matching of argument names is not
  allowed and e.g. `runif(1, mi = 0)` will return a message indicating
  that the full formal name `min` should be used. The default is set via
  the `gradethis.allow_partial_matching` option, or by
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).

## Value

Signals an incorrect grade with feedback if there are differences
between the submitted user code and the solution code. If solution code
is not available, no grade is returned.

## See also

Other grading helper functions:
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md),
[`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md),
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
[`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md).

## Examples

``` r
# Suppose the exercise prompt is to generate 5 random numbers, sampled from
# a uniform distribution between 0 and 1. In this exercise, you know that
# you shouldn't have values outside of the range of 0 or 1, but you'll
# otherwise need to check the submitted code to know that the student has
# chosen the correct sampling function.

grader <-
  # ```{r example-check}
  grade_this({
    fail_if(length(.result) != 5, "I expected 5 numbers.")
    fail_if(
      any(.result < 0 | .result > 1),
      "I expected all numbers to be between 0 and 1."
    )

    # Specific checks passed, but now we want to check the code.
    fail_if_code_feedback()

    # All good!
    pass()
  })
# ```

.solution_code <- "
# ```{r example-check}
  runif(5)
# ```
"

# Not 5 numbers...
grader(mock_this_exercise(runif(1), !!.solution_code))
#> <gradethis_graded: [Incorrect] I expected 5 numbers.>

# Not within [0, 1]...
grader(mock_this_exercise(rnorm(5), !!.solution_code))
#> <gradethis_graded: [Incorrect]
#>   I expected all numbers to be between 0 and 1.
#> >

# Passes specific checks, but hard to tell so check the code...
grader(mock_this_exercise(runif(5, 0.25, 0.75), !!.solution_code))
#> <gradethis_graded: [Incorrect]
#>   In `runif(5, 0.25, 0.75)`, I expected `0` where you wrote
#>   `0.25`.
#> >
grader(mock_this_exercise(rbinom(5, 1, 0.5), !!.solution_code))
#> <gradethis_graded: [Incorrect]
#>   I expected you to call `runif()` where you called
#>   `rbinom()`.
#> >

# Perfect!
grader(mock_this_exercise(runif(n = 5), !!.solution_code))
#> <gradethis_graded: [Correct] Beautiful! Correct!>
```
