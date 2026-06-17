# A checker function to use with learnr

For exercise checking, learnr tutorials require a function that learnr
can use in the background to run the code in each "-check" chunk and to
format the results into a format that learnr can display. To enable
exercise checking in your learnr tutorial, attach gradethis with
[`library(gradethis)`](https://pkgs.rstudio.com/gradethis/), or call
[`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
in the setup chunk of your tutorial. See
[`gradethis_demo()`](https://rstudio.github.io/gradethis/reference/gradethis_demo.md)
to see an example learnr document that uses
`gradethis_exercise_checker()`.

## Usage

``` r
gradethis_exercise_checker(
  label = NULL,
  solution_code = NULL,
  user_code = NULL,
  check_code = NULL,
  envir_result = NULL,
  evaluate_result = NULL,
  envir_prep = NULL,
  last_value = NULL,
  stage = NULL,
  ...,
  solution_eval_fn = NULL
)
```

## Arguments

- label:

  Label for exercise chunk

- solution_code:

  Code provided within the "-solution" chunk for the exercise.

- user_code:

  R code submitted by the user

- check_code:

  Code provided within the "-check" (or "-code-check") chunk for the
  exercise.

- envir_result:

  The R environment after the execution of the chunk.

- evaluate_result:

  The return value from the
  [`evaluate::evaluate`](https://rstudio.github.io/gradethis/reference/evaluate.r-lib.org/reference/evaluate.md)
  function.

- envir_prep:

  A copy of the R environment before the execution of the chunk.

- last_value:

  The last value from evaluating the user's exercise submission.

- stage:

  The current stage of exercise checking.

- ...:

  Extra arguments supplied by learnr

- solution_eval_fn:

  A function taking solution `code` and an `envir` (an environment
  equivalent to `envir_prep`) and that will return the value of the
  evaluated `code`. This callback function allows grading authors to
  write custom solution evaluation functions for non-R exercise engines.
  The result of the evaluated code should be an R object that will be
  accessible to the grading code in
  [.solution](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  or
  [.solution_all](https://rstudio.github.io/gradethis/reference/grade_this-objects.md).

  You may also provide a named list of solution evaluation functions to
  the `gradethis.exercise_checker.solution_eval_fn` global option. The
  names of the list should match the exercise engine for which the
  function should be applied.

  For example, for a hypothetical exercise engine `echo` that simply
  echoes the user's code, you could provide a `solution_eval_fn` that
  also just echoes the solution code:

      options(
        gradethis.exercise_checker.solution_eval_fn = list(
          echo = function(code, envir) {
            code
          }
        )
      )

  Solution evaluation functions should determine if the solution code is
  missing and if so throw an error with class `error_missing_solution`
  (see [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)
  for help throwing this error).

## Value

Returns a feedback object suitable for learnr tutorials with the results
of the exercise grading code.

## See also

[`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md),
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gradethis_demo()
} # }
```
