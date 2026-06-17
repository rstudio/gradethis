# Fail if grading code produces an error

When grading code involves unit-style testing, you may want to use
testthat expectation function to test the user's submitted code. In
these cases, to differentiate between expected errors and internal
errors indicative of issues with the grading code, gradethis requires
that authors wrap assertion-style tests in `fail_if_error()`. This
function catches any errors and converts them into
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
grades. It also makes the error and its message available for use in the
`message` glue string as `.error` and `.error_message` respectively.

## Usage

``` r
fail_if_error(
  expr,
  message = "{.error_message}",
  ...,
  env = parent.frame(),
  hint = TRUE,
  encourage = getOption("gradethis.fail.encourage", FALSE)
)
```

## Arguments

- expr:

  An expression to evaluate that whose errors are safe to be converted
  into failing grades with
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md).

- message:

  A glue string containing the feedback message to be returned to the
  user. Additional `.error` and `.error_message` objects are made
  available for use in the message.

- ...:

  Additional arguments passed to
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  or additional data to be included in the feedback object.

- env:

  environment to evaluate the glue `message`. Most users of gradethis
  will not need to use this argument.

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

## Value

If an error occurs while evaluating `expr`, the error is returned as a
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
grade. Otherwise, no value is returned.

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
# The user is asked to add 2 + 2, but they take a shortcut
ex <- mock_this_exercise("'4'")

# Normally, grading code with an author error returns an internal problem grade
grade_author_mistake <- grade_this({
  if (identical(4)) {
    pass("Great work!")
  }
  fail()
})(ex)
#> #> grade_this({
#> #>     if (identical(4)) {
#> #>         pass("Great work!")
#> #>     }
#> #>     fail()
#> #> })(ex)
#> Error in (function (e) {    on_error(e, this_env)})(structure(list(message = "argument \"y\" is missing, with no default",     call = identical(4)), class = c("getvarError", "missingArgError", "error", "condition"))): argument "y" is missing, with no default

# This returns a "problem occurred" grade
grade_author_mistake
#> <gradethis_graded: [Neutral]
#>   A problem occurred with the grading code for this exercise.
#> >
# ...that also includes information about the error (not shown to users)
grade_author_mistake$error
#> $message
#> [1] "argument \"y\" is missing, with no default"
#> 
#> $call
#> [1] "(function (e) \n{\n    on_error(e, this_env)\n})(structure(list(message = \"argument \\\"y\\\" is missing, with no default\", \n    call = identical(4)), class = c(\"getvarError\", \"missingArgError\", \n\"error\", \"condition\")))"
#> 
#> $gradethis_call
#> [1] "grade_this({\n    if (identical(4)) {\n        pass(\"Great work!\")\n    }\n    fail()\n})(ex)"
#> 

# But sometimes we'll want to use unit-testing helper functions where we know
# that an error is indicative of a problem in the users' code
grade_this({
  fail_if_error({
    testthat::expect_length(.result, 1)
    testthat::expect_true(is.numeric(.result))
    testthat::expect_equal(.result, 4)
  })
  pass("Good job!")
})(ex)
#> <gradethis_graded: [Incorrect]
#>   Expected `is.numeric(.result)` to be TRUE.
#>   Differences:
#>   `actual`: FALSE
#>   `expected`: TRUE
#> >

# Note that you don't need to reveal the error message to the user
grade_this({
  fail_if_error(
    message = "Your result isn't a single numeric value.",
    {
      testthat::expect_length(.result, 1)
      testthat::expect_true(is.numeric(.result))
      testthat::expect_equal(.result, 4)
    }
  )
  pass("Good job!")
})(ex)
#> <gradethis_graded: [Incorrect]
#>   Your result isn't a single numeric value.
#> >
```
