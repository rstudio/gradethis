# Grade student code against a solution

`grade_this_code()` compares student code to a solution (i.e. model
code) and describes the first way in which the student code differs. If
the student code exactly matches the solution, `grade_this_code()`
returns a customizable success message (`correct`). If the student code
does not match the solution, a customizable incorrect message
(`incorrect`) can also be provided.

In most cases, to use `grade_this_code()`, ensure that your exercise has
a `-solution` chunk:

    ```{r example-solution}
    sqrt(log(1))
    ```

Then, call `grade_this_code()` in your exercise's `-check` or
`-code-check` chunk:

    ```{r example-check}
    grade_this_code()
    ```

If `grade_this_code()` is called in a `-code-check` chunk and returns
feedback, either passing or failing feedback, then the user's code is
not executed. If you want the user to see the output of their code, call
`grade_this_code()` in the `-check` chunk. You can also use
`grade_this_code()` as a pre-check to avoid running code when it fails
or passes by calling `grade_this_code()` inside the `-code-check` chunk
and setting `action = "pass"` or `action = "fail"` to only return
feedback when the user's code passes or fails, respectively. (Note:
requires learnr version 0.10.1.9017 or later.)

Learn more about how to use `grade_this_code()` in the **Details**
section below.

## Usage

``` r
grade_this_code(
  correct = getOption("gradethis.code_correct", getOption("gradethis.pass", "Correct!")),
  incorrect = getOption("gradethis.code_incorrect", getOption("gradethis.fail",
    "Incorrect")),
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE),
  action = c("both", "pass", "fail")
)
```

## Arguments

- correct:

  A `glue`-able character string to display if the student answer
  matches a known correct answer.

- incorrect:

  A `glue`-able character string to display if the student answer does
  not match the known correct answer. Use
  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  in this string to control the placement of the auto-generated feedback
  message produced by comparing the student's submission with the
  solution. Use a string that doesn't include
  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  to grade the student's code without providing feedback.

- ...:

  Ignored

- allow_partial_matching:

  A logical. If `FALSE`, the partial matching of argument names is not
  allowed and e.g. `runif(1, mi = 0)` will return a message indicating
  that the full formal name `min` should be used. The default is set via
  the `gradethis.allow_partial_matching` option, or by
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).

- action:

  The action to take:

  1.  `"pass"` provide passing `correct` feedback when the user's code
      matches the solution code.

  2.  `"fail"` provide failing `incorrect` feedback when the user's code
      does not match the solution code.

  3.  `"both"` always provide passing or failing feedback.

## Value

Returns a function whose first parameter will be an environment
containing objects specific to the exercise and submission (see
**Available variables**). For local testing, you can create a version of
the expected environment for a mock exercise submission with
[`mock_this_exercise()`](https://rstudio.github.io/gradethis/reference/mock_this_exercise.md).
Calling the returned function on the exercise-checking environment will
evaluate the grade-checking `expr` and return a final grade via
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md).

## Details

`grade_this_code()` only inspects for code differences between the
student's code and the solution code. The final result of the student
code and solution code is ignored. See the **Code differences** section
of
[`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
for implementation details on how code is determined to be different.

You can call `grade_this_code()` in two ways:

1.  If you want to check the student's code without evaluating it, call
    `grade_this_code()` in the `*-code-check` chunk.

2.  To return grading feedback in along with the resulting output of the
    student's code, call `grade_this_code()` in the `*-check` chunk of
    the exercise.

To provide the solution code, include a `*-solution` code chunk in the
learnr document for the exercise to be checked. When used in this way,
`grade_this_code()` will automatically find and use the student's
submitted code — `.user_code` in
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
— as well as the solution code — `.solution_code` in
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md).

## Custom messages

You can customize the `correct` and `incorrect` messages shown to the
user by `grade_this_code()`. Both arguments accept template strings that
are processed by
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html). If you
provide a custom template string, it completely overwrites the default
string, but you can include the components used by the default message
by adding them to your custom message.

There are four helper functions used in the default messages that you
may want to include in your custom messages. To use the output of any of
the following, include them inside braces in the template string. For
example use `{code_feedback()}` to add the code feedback to your custom
`incorrect` message.

1.  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md):
    Adds feedback about the first observed difference between the
    student's submitted code and the model solution code. If you want to
    grade the student's code without providing feedback, leave
    [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
    out of your string.

2.  [`pipe_warning()`](https://rstudio.github.io/gradethis/reference/pipe_warning.md):
    Informs the user that their code was unpiped prior to comparison.
    This message is included by default to help clarify cases when the
    code feedback makes more sense in the unpiped context.

3.  [`random_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)
    and
    [`random_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md):
    These praising and encouraging messages are included by default in
    correct and incorrect grades, by default.

## See also

[`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md),
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
[`mock_this_exercise()`](https://rstudio.github.io/gradethis/reference/mock_this_exercise.md)

## Examples

``` r
# For an interactive example run: gradethis_demo()

# # These are manual examples, see grading demo for `learnr` tutorial usage

grade_this_code()(
  mock_this_exercise(
    .user_code     = "sqrt(log(2))", # user submitted code
    .solution_code = "sqrt(log(1))"  # from -solution chunk
  )
)
#> <gradethis_graded_this_code: [Incorrect]
#>   In `log(2)`, I expected `1` where you wrote `2`. Don't give
#>   up now, try it one more time.
#> >

grade_this_code()(
  mock_this_exercise(
    # user submitted code
    .user_code     = "runif(1, 0, 10)",
    # from -solution chunk
    .solution_code = "runif(n = 1, min = 0, max = 1)"
  )
)
#> <gradethis_graded_this_code: [Incorrect]
#>   In `runif(1, 0, 10)`, I expected `1` where you wrote `10`.
#>   Try it again. Perseverance is the key to success.
#> >

# By default, grade_this_code() informs the user that piped code is unpiped
# when comparing to the solution
grade_this_code()(
  mock_this_exercise(
    # user submitted code
    .user_code     = "storms %>% select(year, month, hour)",
    # from -solution chunk
    .solution_code = "storms %>% select(year, month, day)"
  )
)
#> <gradethis_graded_this_code: [Incorrect]
#>   I see that you are using pipe operators (e.g. %>%), so I
#>   want to let you know that this is how I am interpreting your
#>   code before I check it:
#> 
#>   ```r
#>   select(storms, year, month, hour)
#>   ```
#> 
#>   In `storms %>% select(year, month, hour)`, I expected `day`
#>   where you wrote `hour`. But no need to fret, try it again.
#> >

# By setting `correct` or `incorrect` you can change the default message
grade_this_code(
  correct = "Good work!",
  incorrect = "Not quite. {code_feedback()} {random_encouragement()}"
)(
  mock_this_exercise(
    # user submitted code
    .user_code     = "storms %>% select(year, month, hour)",
    # from -solution chunk
    .solution_code = "storms %>% select(year, month, day)"
  )
)
#> <gradethis_graded_this_code: [Incorrect]
#>   Not quite. In `storms %>% select(year, month, hour)`, I
#>   expected `day` where you wrote `hour`. That's okay: you
#>   learn more from mistakes than successes. Let's do it one
#>   more time.
#> >
```
