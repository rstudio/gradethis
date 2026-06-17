# Signal a passing or failing grade if two values are equal

`pass_if_equal()`, `fail_if_equal()`, and `fail_if_not_equal()` are
three
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
helper functions that signal a passing or a failing grade based on the
whether two values are equal. They are designed to easily compare the
returned value of the student's submitted code with the value returned
by the solution or another known value:

- Each function finds and uses `.result` as the default for `x`, the
  first item in the comparison. `.result` is the last value returned
  from the user's submitted code.

- `pass_if_equal()` additionally finds and uses `.solution` as the
  default expected value `y`.

See
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
for more information on gradethis grade-signaling functions.

## Usage

``` r
pass_if_equal(
  y = .solution,
  message = getOption("gradethis.pass", "Correct!"),
  x = .result,
  ...,
  env = parent.frame(),
  tolerance = sqrt(.Machine$double.eps),
  praise = getOption("gradethis.pass.praise", FALSE)
)

fail_if_equal(
  y,
  message = getOption("gradethis.fail", "Incorrect"),
  x = .result,
  ...,
  env = parent.frame(),
  tolerance = sqrt(.Machine$double.eps),
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE)
)

fail_if_not_equal(
  y,
  message = getOption("gradethis.fail", "Incorrect"),
  x = .result,
  ...,
  env = parent.frame(),
  tolerance = sqrt(.Machine$double.eps),
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE)
)
```

## Arguments

- y:

  The expected value against which `x` is compared using
  `gradethis_equal(x, y)`.

  In `pass_if_equal()`, if no value is provided, the exercise
  `.solution` (i.e. the result of evaluating the code in the exercise's
  `*-solution` chunk) will be used for the comparison.

  If the exercise uses multiple solutions with *different results*, set
  `y = .solution_all`. In this case, `pass_if_equal()` will test each of
  the solutions and provide a passing grade if `x` matches *any* values
  contained in `y`. Note that if the exercise has multiple solutions but
  they all return the same result, it will be faster to use the default
  value of `y = .solution`.

- message:

  A character string of the message to be displayed. In all grading
  helper functions other than
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md),
  `message` is a template string that will be processed with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- x:

  First item in the comparison. By default, when used inside
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
  `x` is automatically assigned the value of `.result` — in other words
  the result of running the student's submitted code. `x` is not the
  first argument since you will often want to compare the final value of
  the student's submission against a specific value, `y`.

- ...:

  Additional arguments passed to
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)

- env:

  environment to evaluate the glue `message`. Most users of gradethis
  will not need to use this argument.

- tolerance:

  If non-`NULL`, used as threshold for ignoring small floating point
  difference when comparing numeric vectors. Using any non-`NULL` value
  will cause integer and double vectors to be compared based on their
  values, not their types, and will ignore the difference between `NaN`
  and `NA_real_`.

  It uses the same algorithm as
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html), i.e., first we
  generate `x_diff` and `y_diff` by subsetting `x` and `y` to look only
  locations with differences. Then we check that
  `mean(abs(x_diff - y_diff)) / mean(abs(y_diff))` (or just
  `mean(abs(x_diff - y_diff))` if `y_diff` is small) is less than
  `tolerance`.

- praise:

  Include a random praising phrase with
  [`random_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)?
  The default value of `praise` can be set using
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  or the `gradethis.pass.praise` option.

- hint:

  Include a code feedback hint with the failing message? This argument
  only applies to
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  and `fail_if_equal()` and the message is added using the default
  options of
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

Returns a passing or failing grade if `x` and `y` are equal.

## Functions

- `pass_if_equal()`: Signal a *passing* grade only if `x` and `y` are
  equal.

- `fail_if_equal()`: Signal a *failing* grade only if `x` and `y` are
  equal.

- `fail_if_not_equal()`: Signal a *failing* grade if `x` and `y` are
  *not* equal.

## Comparing with Multiple Solutions

If your exercise includes multiple solutions that are variations of the
same task — meaning that all solutions achieve the same result — you can
call `pass_if_equal()` without changing any defaults to compare the
result of the student's submission to the common solution result. After
checking if any solution matches, you can perform additional checks or
you can call
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md) with
the [default
message](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
or with `hint = TRUE`.
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md) will
automatically provide code feedback for the most likely solution.

By default, `pass_if_equal()` will compare
[.result](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
with
[.solution](https://rstudio.github.io/gradethis/reference/grade_this-objects.md),
or the final value returned by the entire `-solution` chunk (in other
words, the last solution). This default behavior covers both exercises
with a single solution and exercises with multiple solutions that all
return the same value.

When your exercise has multiple solutions with **different results**,
`pass_if_equal()` can compare the student's
[.result](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
to each of the solutions in
[.solution_all](https://rstudio.github.io/gradethis/reference/grade_this-objects.md),
returning a passing grade when the result matches any of the values
returned by the set of solutions. You can opt into this behavior by
calling

    pass_if_equal(.solution_all)

Note that this causes `pass_if_equal()` to evaluate each of the
solutions in the set, and may increase the computation time.

Here's a small example. Suppose an exercise asks students to filter
`mtcars` to include only cars with the same number of cylinders.
Students are free to pick cars with 4, 6, or 8 cylinders, and so your
`-solution` chunk would include this code (ignoring the `ex_solution`
variable, the chunk would contain the code in the string below):

    ex_solution <- "
    # four cylinders ----
    mtcars[mtcars$cyl == 4, ]

    # six cylinders ----
    mtcars[mtcars$cyl == 6, ]

    # eight cylinders ----
    mtcars[mtcars$cyl == 8, ]
    "

In the `-check` chunk, you'd call
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
and ask `pass_if_equal()` to compare the student's
[.result](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
to
[.solution_all](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
(all the solutions).

    ex_check <- grade_this({
      pass_if_equal(
        y = .solution_all,
        message = "The cars in your result all have {.solution_label}!"
       )

      fail()
    })

What happens when a student submits one of these solutions? This
function below mocks the process of a student submitting an attempt.

    student_submits <- function(code) {
      withr::local_seed(42)
      submission <- mock_this_exercise(!!code, !!ex_solution)
      ex_check(submission)
    }

If they submit code that returns one of the three possible solutions,
they receive positive feedback.

    student_submits("mtcars[mtcars$cyl == 4, ]")
    #> <gradethis_graded: [Correct]
    #>   The cars in your result all have four cylinders!
    #> >
    student_submits("mtcars[mtcars$cyl == 6, ]")
    #> <gradethis_graded: [Correct]
    #>   The cars in your result all have six cylinders!
    #> >

Notice that the solution label appears in the feedback message. When
`pass_if_equal()` picks a solution as correct, three variables are made
available for use in the glue string provided to `message`:

- `.solution_label`: The heading label of the matching solution

- `.solution_code`: The code of the matching solution

- `.solution`: The value of the evaluated matching solution code

If the student submits incorrect code, `pass_if_equal()` defers to later
grading code.

    student_submits("mtcars[mtcars$cyl < 8, ]")
    #> <gradethis_graded: [Incorrect]
    #>   Incorrect. In `mtcars[mtcars$cyl < 8, ]`, I expected you to call `==`
    #>   where you called `<`. Please try again.
    #> >

Here, because
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
provides
[`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
by default, and because
[`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
is also aware of the multiple solutions for this exercise, the code
feedback picks the *eight cylinders* solution and gives advice based on
that particular solution.

## See also

Other grading helper functions:
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md),
[`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md),
`pass_if_equal()`, `fail_if_equal()`.

## Examples

``` r
# Suppose our prompt is to find the cars in `mtcars` with 6 cylinders...

grader <-
  # ```{r example-check}
  grade_this({
    # Automatically pass if .result equal to .solution
    pass_if_equal()

    fail_if_equal(mtcars[mtcars$cyl == 4, ], message = "Not four cylinders")
    fail_if_equal(mtcars[mtcars$cyl == 8, ], message = "Not eight cylinders")

    # Default to failing grade with feedback
    fail()
  })
# ```

.solution <-
  # ```{r example-solution}
  mtcars[mtcars$cyl == 6, ]
# ```

# Correct!
grader(mock_this_exercise(mtcars[mtcars$cyl == 6, ], !!.solution))
#> <gradethis_graded: [Correct] Wonderful! Correct!>

# These fail with specific messages
grader(mock_this_exercise(mtcars[mtcars$cyl == 4, ], !!.solution))
#> <gradethis_graded: [Incorrect] Not four cylinders>
grader(mock_this_exercise(mtcars[mtcars$cyl == 8, ], !!.solution))
#> <gradethis_graded: [Incorrect] Not eight cylinders>

# This fails with default feedback message
grader(mock_this_exercise(mtcars[mtcars$mpg == 8, ], !!.solution))
#> <gradethis_graded: [Incorrect]
#>   Incorrect. I expected you to call `structure()` where you
#>   called `[`. Please try again.
#> >
```
