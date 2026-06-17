# Grade a student's submission using custom logic

`grade_this()` allows instructors to write custom logic to evaluate,
grade and give feedback to students. To use `grade_this()`, call it
directly in your `*-check` chunk:

    ```{r example-check}
    grade_this({
      # custom checking code appears here
      if (identical(.result, .solution)) {
        pass("Great work!")
      }
      fail("Try again!")
    })
    ```

`grade_this()` makes available a number of objects based on the exercise
and the student's submission that can be used to evaluate the student's
submitted code. See
[`?"grade_this-objects"`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
for more information about these objects.

As the instructor, you are free to use any logic to determine a
student's grade as long as a
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
object is signaled. The check code can also contain testthat expectation
code. Failed testthat expectations will be turned into
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)ed
grades with the corresponding message.

A final grade is signaled from `grade_this()` using the
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
helper functions, which include
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md),
among others. `grade_this()` uses condition handling to short-circuit
further evaluation when a grade is reached. This means that you may also
signal a failing grade using any of the `expect_*()` functions from
testthat, other functions designed to work with testthat, such as
checkmate, or standard R errors via
[`stop()`](https://rdrr.io/r/base/stop.html). Learn more about this
behavior in
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md) in
the section **Return a grade immediately**.

## Usage

``` r
grade_this(
  expr,
  ...,
  maybe_code_feedback = getOption("gradethis.maybe_code_feedback", TRUE)
)
```

## Arguments

- expr:

  The grade-checking expression to be evaluated. This expression must
  either signal a grade via
  [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) or
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  functions or their sibling functions.

  By default, errors in this expression are converted to "internal
  problem" grades that mask the error for the user. If your grading
  logic relies on unit-test-styled functions, such as those from
  testthat, you can use
  [`fail_if_error()`](https://rstudio.github.io/gradethis/reference/fail_if_error.md)
  to convert errors into
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  grades.

- ...:

  Ignored

- maybe_code_feedback:

  Should
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  provide code feedback when used in a
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  message? The default value can be set with
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).

  Typically,
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  is called in the default
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  message (the default can be customized the `fail` argument of
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)).
  If the `maybe_code_feedback` argument is `FALSE`,
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  returns an empty string.

## Value

Returns a function whose first parameter will be an environment
containing objects specific to the exercise and submission (see
**Available variables**). For local testing, you can create a version of
the expected environment for a mock exercise submission with
[`mock_this_exercise()`](https://rstudio.github.io/gradethis/reference/mock_this_exercise.md).
Calling the returned function on the exercise-checking environment will
evaluate the grade-checking `expr` and return a final grade via
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md).

## See also

[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md),
[`mock_this_exercise()`](https://rstudio.github.io/gradethis/reference/mock_this_exercise.md),
[`gradethis_demo()`](https://rstudio.github.io/gradethis/reference/gradethis_demo.md)

## Examples

``` r
# For an interactive example run: gradethis_demo()

# Suppose we have an exercise that prompts students to calculate the
# average height of Loblolly pine trees using the `Loblolly` data set.
# We might write an exercise `-check` chunk like the one below.
#
# Since grade_this() returns a function, we'll save the result of this
# "chunk" as `grader()`, which can be called on an exercise submission
# to evaluate the student's code, which we'll simulate with
# `mock_this_exercise()`.

grader <-
  # ```{r example-check}
  grade_this({
    if (length(.result) != 1) {
      fail("I expected a single value instead of {length(.result)} values.")
    }

    if (is.na(.result)) {
      fail("I expected a number, but your code returned a missing value.")
    }

    avg_height <- mean(Loblolly$height)
    if (identical(.result, avg_height)) {
      pass("Great work! The average height is {round(avg_height, 2)}.")
    }

    # Always end grade_this() with a default grade.
    # By default fail() will also give code feedback,
    # if a solution is available.
    fail()
  })
# ```

# Simulate an incorrect answer: too many values...
grader(mock_this_exercise(.user_code = Loblolly$height[1:2]))
#> <gradethis_graded: [Incorrect]
#>   I expected a single value instead of 2 values.
#> >

# This student submission returns a missing value...
grader(mock_this_exercise(mean(Loblolly$Seed)))
#> Warning: argument is not numeric or logical: returning NA
#> <gradethis_graded: [Incorrect]
#>   I expected a number, but your code returned a missing value.
#> >
# This student submission isn't caught by any specific tests,
# the final grade is determined by the default (last) value in grade_this()
grader(mock_this_exercise(mean(Loblolly$age)))
#> <gradethis_graded: [Incorrect]
#>   Incorrect. But no need to fret, try it again.
#> >

# If you have a *-solution chunk,
# fail() without arguments gives code feedback...
grader(
  mock_this_exercise(
    .user_code = mean(Loblolly$age),
    .solution_code = mean(Loblolly$height)
  )
)
#> <gradethis_graded: [Incorrect]
#>   Incorrect. In `Loblolly$age`, I expected `height` where you
#>   wrote `age`. Try it again. I have a good feeling about this.
#> >

# Finally, the "student" gets the correct answer!
grader(mock_this_exercise(mean(Loblolly$height)))
#> <gradethis_graded: [Correct]
#>   Great work! The average height is 32.36.
#> >
```
