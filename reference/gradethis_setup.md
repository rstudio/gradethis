# Setup gradethis for use within learnr

To use gradethis in your learnr tutorial, you only need to call
[`library(gradethis)`](https://pkgs.rstudio.com/gradethis/) in your
tutorial's setup chunk.

    ```{r setup}
    library(learnr)
    library(gradethis)
    ```

Use `gradethis_setup()` to change the default options suggested by
gradethis. This function also describes in detail each of the global
options available for customization in the gradethis package. Note that
you most likely do not want to change the defaults values for the learnr
tutorial options that are prefixed with `exercise.`. Each of the
gradethis-specific arguments sets a global option with the same name,
prefixed with `gradethis.`. For example, `pass` sets `gradethis.pass`.

## Usage

``` r
gradethis_setup(
  pass = NULL,
  fail = NULL,
  ...,
  code_correct = NULL,
  code_incorrect = NULL,
  maybe_code_feedback = NULL,
  maybe_code_feedback.before = NULL,
  maybe_code_feedback.after = NULL,
  pass.praise = NULL,
  fail.hint = NULL,
  fail.encourage = NULL,
  pipe_warning = NULL,
  grading_problem.message = NULL,
  grading_problem.type = NULL,
  error_checker.message = NULL,
  allow_partial_matching = NULL,
  exercise.checker = gradethis_exercise_checker,
  exercise.timelimit = NULL,
  compare_timelimit = NULL,
  exercise.error.check.code = NULL,
  fail_code_feedback = NULL
)
```

## Arguments

- pass:

  Default message for
  [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md).
  Sets `options("gradethis.pass")`

- fail:

  Default message for
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md).
  Sets `options("gradethis.fail")`

- ...:

  Arguments passed on to
  [`learnr::tutorial_options`](https://pkgs.rstudio.com/learnr/reference/tutorial_options.html)

  `exercise.cap`

  :   Caption for exercise chunk (defaults to the engine's icon or the
      combination of the engine and `" code"`).

  `exercise.eval`

  :   Whether to pre-evaluate the exercise so the reader can see some
      default output (defaults to `FALSE`).

  `exercise.lines`

  :   Lines of code for exercise editor (defaults to the number of lines
      in the code chunk).

  `exercise.pipe`

  :   The characters to enter when the user presses the "Insert Pipe"
      keyboard shortcut in the exercise editor (`Ctrl/Cmd + Shift + M`).
      This can be set at the tutorial level or for an individual
      exercise. If `NULL` (default), the base R pipe (`|>`) is used when
      the tutorial is rendered in R \>= 4.1.0, otherwise the magrittr
      pipe (`%>%`) is used.

  `exercise.blanks`

  :   A regular expression to be used to identify blanks in submitted
      code that the user should fill in. If `TRUE` (default), blanks are
      three or more underscores in a row. If `FALSE`, blank checking is
      not performed.

  `exercise.completion`

  :   Use code completion in exercise editors.

  `exercise.diagnostics`

  :   Show diagnostics in exercise editors.

  `exercise.startover`

  :   Show "Start Over" button on exercise.

  `exercise.reveal_solution`

  :   Whether to reveal the exercise solution if a solution chunk is
      provided.

- code_correct:

  Default `correct` message for
  [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md).
  If unset,
  [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
  falls back to the value of the `gradethis.pass` option. Sets the
  `gradethis.code_correct` option.

- code_incorrect:

  Default `incorrect` message for
  [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md).
  If unset
  [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
  falls back to the value of the `gradethis.fail` option. Sets the
  `gradethis.code_incorrect` option.

- maybe_code_feedback:

  Logical `TRUE` or `FALSE` to determine whether
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  should return code feedback, where if `FALSE`,
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  will return an empty string.
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  is used in the default messages when
  [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) or
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  are called without any arguments, which are set by the `pass` or
  `fail` arguments of `gradethis_setup()`.

- maybe_code_feedback.before, maybe_code_feedback.after:

  Text that should be added `before` or `after` the
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  output, if any is returned. Sets the default values of the `before`
  and `after` arguments of
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md).

- pass.praise:

  Logical `TRUE` or `FALSE` to determine whether a praising phrase
  should be automatically prepended to any
  [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) or
  [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  messages. Sets the `gradethis.pass.praise` option.

- fail.hint:

  Logical `TRUE` or `FALSE` to determine whether an automated code
  feedback hint should be shown with a
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md) or
  [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  message. Sets the `gradethis.fail.hint` option.

- fail.encourage:

  Logical `TRUE` or `FALSE` to determine whether an encouraging phrase
  should be automatically appended to any
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md) or
  [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  messages. Sets the `gradethis.fail.encourage` option.

- pipe_warning:

  The default message used in
  [`pipe_warning()`](https://rstudio.github.io/gradethis/reference/pipe_warning.md).
  Sets the `gradethis.pipe_warning` option.

- grading_problem.message:

  The feedback message used when a grading error occurs. Sets the
  `gradethis.grading_problem.message` option.

- grading_problem.type:

  The feedback type used when a grading error occurs. Must be one of
  `"success"`, `"info"`, `"warning"` (default), `"error"`, or
  `"custom"`. Sets the `gradethis.grading_problem.type` option.

- error_checker.message:

  The default message used by gradethis's default error checker,
  [`gradethis_error_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_error_checker.md).
  Sets the `gradethis.error_checker.message` option.

- allow_partial_matching:

  Logical `TRUE` or `FALSE` to determine whether partial matching is
  allowed in
  [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md).
  Sets the `gradethis.allow_partial_matching` option.

- exercise.checker:

  Function used to check exercise answers (e.g.,
  [`gradethis::grade_learnr()`](https://rstudio.github.io/gradethis/reference/gradethis-deprecated.md)).

- exercise.timelimit:

  Number of seconds to limit execution time to (defaults to `30`).

- compare_timelimit:

  [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  and
  [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  call
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html)
  internally. This helps ensure an accurate comparison, but sometimes
  takes a long time. `compare_timelimit` is the time limit in seconds
  for the execution of
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html)
  (defaults to 80% of `exercise.timelimit`). If the time limit is
  exceeded, [`identical()`](https://rdrr.io/r/base/identical.html) is
  used instead of
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html).

- exercise.error.check.code:

  A string containing R code to use for checking code when an exercise
  evaluation error occurs (e.g., `"gradethis::grade_code()"`).

- fail_code_feedback:

  Deprecated. Use `maybe_code_feedback`.

## Value

Invisibly returns the global options as they were prior to setting them
with `gradethis_setup()`.

## Global package options

These global package options can be set by `gradethis_setup()` or by
directly setting the global option. The default values set for each
option when gradethis is loaded are shown below.

|  |  |
|----|----|
| Option | Default Value |
| `gradethis.pass` | `"{gradethis::random_praise()} Correct!"` |
| `gradethis.pass.praise` | `FALSE` |
| `gradethis.fail` | `"Incorrect.{gradethis::maybe_code_feedback()} {gradethis::random_encouragement()}"` |
| `gradethis.fail.hint` | `FALSE` |
| `gradethis.fail.encourage` | `FALSE` |
| `gradethis.maybe_code_feedback` | `TRUE` |
| `gradethis.maybe_code_feedback.before` | `" "` |
| `gradethis.maybe_code_feedback.after` | `NULL` |
| `gradethis.code_correct` | `NULL` |
| `gradethis.code_incorrect` | `"{gradethis::pipe_warning()}{gradethis::code_feedback()} {gradethis::random_encouragement()}"` |
| `gradethis.pipe_warning` | ```` "I see that you are using pipe operators (e.g. %>%), so I want to let you know that this is how I am interpreting your code before I check it:\n\n```r\n{.user_code_unpiped}\n```\n\n" ```` |
| `gradethis.grading_problem.message` | `"A problem occurred with the grading code for this exercise."` |
| `gradethis.grading_problem.type` | `"warning"` |
| `gradethis.allow_partial_matching` | `NULL` |
| `gradethis.error_checker.message` | ```` "An error occurred with your code:\n\n```\n{.error$message}\n```\n\n\n" ```` |
| `gradethis.compare_timelimit` | `NULL` |

## See also

[`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md)

## Examples

``` r
# Not run in package documentation because this function changes global opts
if (FALSE) {
  old_opts <- gradethis_setup(
    pass = "Great work!",
    fail = "{random_encouragement()}"
  )
}

# Use getOption() to see the default value
getOption("gradethis.pass")
#> [1] "{gradethis::random_praise()} Correct!"
getOption("gradethis.maybe_code_feedback")
#> [1] TRUE
```
