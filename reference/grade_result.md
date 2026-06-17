# Grade result of exercise code (Legacy)

**\[superseded\]** Please use
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
mixed with
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md),
and/or
[`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md).

`grade_result()` and `grade_result_strict()` both take a set of
[`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)/[`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
conditions, evaluate them, and return a final
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
object. For `grade_result_strict()` to return a correct grade, every
[`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
condition must be met, and every
[`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
condition must not be met. On the other hand, `grade_result()`'s final
grade reflects the first satisfied condition (if no conditions are met,
the final grade can be controlled by `default_correct` and
`default_message`).

## Usage

``` r
grade_result(
  ...,
  correct = NULL,
  incorrect = NULL,
  glue_correct = getOption("gradethis.glue_correct"),
  glue_incorrect = getOption("gradethis.glue_incorrect"),
  default_correct = "auto",
  default_message = NULL,
  grader_args = deprecated(),
  learnr_args = deprecated()
)

grade_result_strict(
  ...,
  correct = NULL,
  incorrect = NULL,
  glue_correct = getOption("gradethis.glue_correct_test"),
  glue_incorrect = getOption("gradethis.glue_incorrect_test"),
  grader_args = deprecated(),
  learnr_args = deprecated()
)

condition(x, message, correct)
```

## Arguments

- ...:

  [`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)/[`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  `condition()`s to check.

- correct:

  logical whether the condition is the correct answer.

- incorrect:

  A character string to display if the student answer matches a known
  incorrect answer.

- glue_correct:

  A glue string that returns the final correct message displayed.
  Defaults to `getOption("gradethis_glue_correct")`.

- glue_incorrect:

  A glue string that returns the final incorrect message displayed.
  Defaults to `getOption("gradethis_glue_incorrect")`.

- default_correct:

  In the event that no `condition()`s are met, should the end result be
  correct? When `"auto"`, this will be `TRUE` when all the
  [`conditions()`](https://rdrr.io/r/base/conditions.html) are
  [`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  (and `FALSE` otherwise).

- default_message:

  In the event that no `condition()`s are met, what message should be
  included with the returned
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  object?

- grader_args:

  A list of parameters passed to `grader` functions (provided by
  [`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md)).
  This contains:

  - `user_quo`: Quoted R code submitted by the user. For example
    [`rlang::quo(1)`](https://rlang.r-lib.org/reference/defusing-advanced.html)

  - `solution_quo`: (Optional) Quoted solution R code provided by the
    `*-solution` chunk for an exercise.

- learnr_args:

  A list of all parameters passed to
  [`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md)
  by `learnr`. See
  <https://rstudio.github.io/learnr/exercises.html#exercise_checking>
  for more details.

- x:

  A formula, function, or value, that returns `TRUE` or `FALSE`. When
  comparing objects that are greater than length 1 (e.g., vectors,
  dataframes, matrices, etc) A logical vector will be returned if the
  user uses `==`, not a single logical value. `gradethis` will run the
  vector through `all(..., na.rm = TRUE)` to check for the logical
  value. It is advised that the user use
  [`identical()`](https://rdrr.io/r/base/identical.html) instead of `==`
  in this case.

- message:

  character string for message returned (usually passed in from
  [`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  or
  [`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md).

## Value

a function whose first parameter should be an environment that contains
all necessary information to compare the user's result. The result of
the returned function will be a
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
object containing a formatted `correct` or `incorrect` message.

## See also

[`grade_code()`](https://rstudio.github.io/gradethis/reference/grade_code.md)

## Examples

``` r

# The student submits code that returns `5`
submitted_5 <- mock_this_exercise(.user_code = 2 + 3)

# grade_result() returns a function that is called on the student's
# submission. The *-check chunk of the exercise might contain the following
# grade_result() code. When you're writing your tutorial, you can ignore the
# `(submitted_5)`, we're using that notation here to preview what this
# `grade_result()` will do with the mock submission.
grade_result(
  fail_if(~ identical(.result, 4), "Try adding 1"),
  pass_if(~ identical(.result, 5), "You got 5, great!"),
  fail_if(~ TRUE, "Some generic failing message.")
)(submitted_5)
#> <gradethis_graded: [Correct] Wonderful! You got 5, great! >

grade_result_strict(
  pass_if(~ identical(.result, 5), "You got 5, great!"),
  fail_if(~ !is.integer(.result), "I expected an integer")
)(submitted_5)
#> <gradethis_graded: [Incorrect]
#>   1/2 correct! Let's try it again.
#> >

# Suppose our exercise asks the student to write a function that adds 1 to
# the value of its only argument. The student submits:
submitted_fn <- mock_this_exercise(
  .user_code = function(x) { x + 2 }
)

# We can check submissions for this exercise using `grade_result()`. Note that
# because all checks in this `grade_result()` are `fail_if()` statements, if
# the student's submission passes all of the checks, then `grade_result()`
# will return a correct grade.
grade_result(
  fail_if(~ !is.function(.result), "I expected a function."),
  fail_if(~ .result(1) != 2, "Your function should add one.")
)(submitted_fn)
#> <gradethis_graded: [Incorrect]
#>   Your function should add one.  Try it again. Perseverance is
#>   the key to success.
#> >

# To learn more about using grade_result() and grade_code() with learnr, see:
if (FALSE) { # \dontrun{
gradethis_demo()
} # }
```
