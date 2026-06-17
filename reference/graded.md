# Signal a final grade for a student's submission

`graded()` is used to signal a final grade for a submission. Most
likely, you'll want to use its helper functions: `pass()`, `fail()`,
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
[`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
[`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
and
[`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md).
When used in
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
these functions signal a final grade and no further checking of the
student's submitted code is performed. See the sections below for more
details about how these functions are used in
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md).

## Usage

``` r
graded(correct, message = NULL, ..., type = NULL, location = NULL)

pass(
  message = getOption("gradethis.pass", "Correct!"),
  ...,
  env = parent.frame(),
  praise = getOption("gradethis.pass.praise", FALSE)
)

fail(
  message = getOption("gradethis.fail", "Incorrect"),
  ...,
  env = parent.frame(),
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE)
)
```

## Arguments

- correct:

  A logical value of whether or not the checked code is correct.

- message:

  A character string of the message to be displayed. In all grading
  helper functions other than `graded()`, `message` is a template string
  that will be processed with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- ...:

  Additional arguments passed to `graded()` or additional data to be
  included in the feedback object.

- type, location:

  The `type` and `location` of the feedback object provided to learnr.
  See <https://rstudio.github.io/learnr/exercises.html#Custom_checking>
  for more details.

  `type` may be one of "auto", "success", "info", "warning", "error", or
  "custom".

  `location` may be one of "append", "prepend", or "replace".

- env:

  environment to evaluate the glue `message`. Most users of gradethis
  will not need to use this argument.

- praise:

  Include a random praising phrase with
  [`random_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)?
  The default value of `praise` can be set using
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  or the `gradethis.pass.praise` option.

- hint:

  Include a code feedback hint with the failing message? This argument
  only applies to `fail()` and
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

`pass()` signals a *correct* submission, `fail()` signals an *incorrect*
submission, and `graded()` returns a correct or incorrect submission
according to the value of `correct`.

## Functions

- `graded()`: Prepare and signal a graded result.

- `pass()`: Signal a *passing* grade.

- `fail()`: Signal a *failing* grade.

## Usage in [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)

The `graded()` helper functions are all designed to be called from
within
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
but this has the unfortunate side-effect of making their default
arguments somewhat opaque.

The helper functions follow these common patterns:

1.  If you don't provide a custom `message`, the default pass or fail
    messages will be used. With the default gradethis setup, the pass
    message follows the pattern `{gradethis::random_praise()} Correct!`
    , and the fail message follows
    `Incorrect.{gradethis::maybe_code_feedback()} {gradethis::random_encouragement()}`.

    You can set the default message pattern using the `pass` and `fail`
    in
    [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md),
    or the options `gradethis.pass` and `gradethis.fail`.

    In the custom `message`, you can use
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
    syntax to reference any of the available variables in
    [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
    or that you've created in your checking code: e.g.
    `"Your table has {nrow(.result)} rows."`.

2.  [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
    and
    [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
    automatically compare their first argument against the `.result` of
    running the student's code.
    [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
    takes this one step further and if called without any arguments will
    compare the `.result` to the value returned by evaluating the
    `.solution` code, if available.

3.  All `fail` helper functions have an additional `hint` parameter. If
    `hint = TRUE`, a code feedback hint is added to the custom
    `message`. You can also control `hint` globally with
    [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).

4.  All helper functions include an `env` parameter, that you can
    generally ignore. It's used internally to help `pass()` and `fail()`
    *et al.* find the default argument values and to build the `message`
    using
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## Return a grade immediately

`graded()` and its helper functions are designed to short-circuit
further evaluation whenever they are called. If you're familiar with
writing functions in R, you can think of `graded()` (and `pass()`,
`fail()`, etc.) as a special version of
[`return()`](https://rdrr.io/r/base/function.html). If a grade is
created, it is returned immediately and no more checking will be
performed.

The immediate return behavior can be helpful when you have to perform
complicated or long-running tests to determine if a student's code
submission is correct. We recommend that you perform the easiest tests
first, progressing to the most complicated tests. By taking advantage of
early grade returns, you can simplify your checking code:

    ```{r}
    grade_this({
      # is the answer a tibble?
      if (!inherits(.result, "tibble")) {
        fail("Your answer should be a tibble.")
      }

      # from now on we know that .result is a tibble...
      if (nrow(.result) != 5 && ncol(.result) < 2) {
        fail("Your table should have 5 rows and more than 1 column.")
      }

      # ...and now we know it has 5 rows and at least 2 columns
      if (.result[[2]][[5]] != 5) {
        fail("The value of the 5th row of the 2nd column should be 5.")
      }

      # all of the above checks have passed now.
      pass()
    })
    ```

Notice that it's important to choose a final fallback grade as the last
value in your
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
checking code. This last value is the default grade that will be given
if the submission passes all other checks. If you're using the standard
[`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
and you call `pass()` or `fail()` without arguments, `pass()` will
return a random praising phrase and `fail()` will return code feedback
(if possible) with an encouraging phrase.

## See also

Other grading helper functions: `graded()`, `pass()`, `fail()`,
[`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md),
[`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md),
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
[`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md).

## Examples

``` r
# Suppose our exercise asks the student to prepare and execute code that
# returns the value `42`. We'll use `grade_this()` to check their
# submission.
#
# Because we are demonstrating these functions inside R documentation, we'll
# save the function returned by `grade_this()` as `grader()`. Calling
# `grader()` on a mock exercise submission is equivalent to running the
# check code when the student clicks "Submit Answer" in a learnr tutorial.

grader <-
  # ```{r example-check}
  grade_this({
    # Automatically use .result to compare to an expected value
    pass_if_equal(42, "Great work!")

    # Similarly compare .result to an expected wrong value
    fail_if_equal(41, "You were so close!")
    fail_if_equal(43, "Oops, a little high there!")

    # or automatically pass if .result is equal to .solution
    pass_if_equal(message = "Great work!")

    # Be explicit if you need to round to avoid numerical accuracy issues
    pass_if_equal(x = round(.result), y = 42, "Close enough!")
    fail_if_equal(x = round(.result), y = 64, "Hmm, that's not right.")

    # For more complicated calculations, call pass() or fail()
    if (.result > 100) {
      fail("{.result} is way too high!")
    }
    if (.result * 100 == .solution) {
      pass("Right answer, but {.result} is two orders of magnitude too small.")
    }

    # Fail with a hint if student code differs from the solution
    # (Skipped automatically if there isn't a -solution chunk)
    fail_if_code_feedback()

    # Choose a default grade if none of the above have resulted in a grade
    fail()
  })
# ```

# Now lets try with a few different student submissions ----

# Correct!
grader(mock_this_exercise(.user_code = 42))
#> <gradethis_graded: [Correct] Great work!>

# These were close...
grader(mock_this_exercise(.user_code = 41))
#> <gradethis_graded: [Incorrect] You were so close!>
grader(mock_this_exercise(.user_code = 43))
#> <gradethis_graded: [Incorrect] Oops, a little high there!>

# Automatically use .solution if you have a *-solution chunk...
grader(mock_this_exercise(.user_code = 42, .solution_code = 42))
#> <gradethis_graded: [Correct] Great work!>

# Floating point arithmetic is tricky...
grader(mock_this_exercise(.user_code = 42.000001, .solution_code = 42))
#> <gradethis_graded: [Correct] Close enough!>
grader(mock_this_exercise(.user_code = 64.123456, .solution_code = 42))
#> <gradethis_graded: [Incorrect] Hmm, that's not right.>

# Complicated checking situations...
grader(mock_this_exercise(.user_code = 101, .solution_code = 42))
#> <gradethis_graded: [Incorrect] 101 is way too high!>
grader(mock_this_exercise(.user_code = 0.42, .solution_code = 42))
#> <gradethis_graded: [Correct]
#>   Right answer, but 0.42 is two orders of magnitude too small.
#> >

# Finally fall back to the final answer...
grader(mock_this_exercise(.user_code = "20 + 13", .solution_code = "20 + 22"))
#> <gradethis_graded: [Incorrect]
#>   In `20 + 13`, I expected `22` where you wrote `13`.
#> >
```
