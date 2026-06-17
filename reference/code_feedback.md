# Provide automated code feedback

Generate a message describing the first instance of a code mismatch.
Three functions are provided for working with code feedback:
`code_feedback()` does the comparison and returns a character
description of the mismatch, or a `NULL` if no differences are found.
`maybe_code_feedback()` is designed to be used inside
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md) and
related
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
messages, as in `"{maybe_code_feedback()}"`. And `give_code_feedback()`
gives you a way to add code feedback to any
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
message in a
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
or
[`grade_result()`](https://rstudio.github.io/gradethis/reference/grade_result.md)
checking function.

## Usage

``` r
code_feedback(
  user_code = .user_code,
  solution_code = .solution_code_all,
  user_env = .envir_result,
  solution_env = .envir_solution,
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE)
)

maybe_code_feedback(
  user_code = get0(".user_code", parent.frame()),
  solution_code = get0(".solution_code_all", parent.frame()),
  user_env = get0(".envir_result", parent.frame(), ifnotfound = parent.frame()),
  solution_env = get0(".envir_solution", parent.frame(), ifnotfound = parent.frame()),
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE),
  default = "",
  before = getOption("gradethis.maybe_code_feedback.before", " "),
  after = getOption("gradethis.maybe_code_feedback.after", NULL),
  space_before = deprecated(),
  space_after = deprecated()
)

give_code_feedback(
  expr,
  ...,
  env = parent.frame(),
  location = c("after", "before")
)
```

## Arguments

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

- user_env:

  Environment used to standardize formals of the user code. Defaults to
  retrieving
  [.envir_result](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  from the calling environment. If not found, the
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) will be
  used.

- solution_env:

  Environment used to standardize formals of the solution code. Defaults
  to retrieving
  [.envir_solution](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  from the calling environment. If not found, the
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) will be
  used.

- ...:

  Ignored in `code_feedback()` and `maybe_code_feedback()`. In
  `give_code_feedback()`, `...` are passed to `maybe_code_feedback()`.

- allow_partial_matching:

  A logical. If `FALSE`, the partial matching of argument names is not
  allowed and e.g. `runif(1, mi = 0)` will return a message indicating
  that the full formal name `min` should be used. The default is set via
  the `gradethis.allow_partial_matching` option, or by
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).

- default:

  Default value to return if no code feedback is found or code feedback
  can be provided.

- before, after:

  Strings to be added before or after the code feedback message to
  ensure the message is properly formatted in your feedback.

- space_before, space_after:

  Deprecated. Use `before` and `after`.

- expr:

  A grading function — like
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  or
  [`grade_result()`](https://rstudio.github.io/gradethis/reference/grade_result.md)
  — or a character string. The code feedback will be appended to the
  message of any incorrect grades using `maybe_code_feedback()`, set to
  always include the code feedback, if possible. If `expr` is a
  character string, `"{maybe_code_feedback()}"` is pasted into the
  string, without customization.

- env:

  Environment used to standardize formals of the user and solution code.
  Defaults to retrieving
  [.envir_result](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  and
  [.envir_solution](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  from [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html).

- location:

  Should the code feedback message be added before or after?

## Value

- `code_feedback()` returns a character value describing the difference
  between the student's submitted code and the solution. If no
  discrepancies are found, `code_feedback()` returns `NULL`.

- `maybe_code_feedback()` always returns a string for safe use in glue
  strings. If no discrepancies are found, it returns an empty string.

- `give_code_feedback()` catches
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  grades and adds code feedback to the feedback message using
  `maybe_code_feedback()`.

## Functions

- `code_feedback()`: Determine code feedback by comparing the user's
  code to the solution.

- `maybe_code_feedback()`: Return `code_feedback()` result when
  possible. Useful when setting default
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  glue messages. For example, if there is no solution, no code feedback
  will be given.

- `give_code_feedback()`: Appends `maybe_code_feedback()` to the message
  generated by incorrect grades.

## Code differences

There are many different ways that code can be different, yet still the
same. Here is how we detect code differences:

1.  If the single values are different. Ex: `log(2)` vs `log(3)`

2.  If the function call is different. Ex: `log(2)` vs `sqrt(2)`

3.  Validate the user code can be standardized via
    [`rlang::call_standardise()`](https://rlang.r-lib.org/reference/call_standardise.html).
    The `env` parameter is important for this step as gradethis does not
    readily know about user defined functions. Ex:
    `read.csv("file.csv")` turns into `read.csv(file = "file.csv")`

4.  If multiple formals are matched. Ex: `read.csv(f = "file.csv")` has
    `f` match to `file` and `fill`.

5.  Verify that every named argument in the solution appears in the user
    code. Ex: If the solution is `read.csv("file.csv", header = TRUE)`,
    `header` must exist.

6.  Verify that the user did not supply extra named arguments to `...`.
    Ex: `mean(x = 1:10, na.rm = TRUE)` vs `mean(x = 1:10)`

7.  Verify that every named argument in the solution matches the value
    of the corresponding user argument. Ex:
    `read.csv("file.csv", header = TRUE)` vs
    `read.csv("file.csv", header = FALSE)`

8.  Verify that the remaining arguments of the user and solution code
    match in order and value. Ex: `mean(1:10, 0.1)` vs `mean(1:10, 0.2)`

## Examples

``` r
# code_feedback() ------------------------------------------------------

# Values are same, so no differences found
code_feedback("log(2)", "log(2)")
#> NULL

# Functions are different
code_feedback("log(2)", "sqrt(2)")
#> I expected you to call `sqrt()` where you called `log()`.

# Standardize argument names (no differences)
code_feedback("read.csv('file.csv')", "read.csv(file = 'file.csv')")
#> NULL

# Partial matching is not allowed
code_feedback("read.csv(f = 'file.csv')", "read.csv(file = 'file.csv')")
#> `read.csv()` accepts more than one argument name that begins with `f`. As a result, R cannot figure out which argument you want to pass `"file.csv"` to. Check how you spelled `f`, or write out the full argument name.

# Feedback will spot differences in argument values...
code_feedback(
  "read.csv('file.csv', header = FALSE)",
  "read.csv('file.csv', header = TRUE)"
)
#> In `read.csv("file.csv", header = FALSE)`, I expected `header = TRUE` where you wrote `header = FALSE`.

# ...or when arguments are expected to appear in a call...
code_feedback("mean(1:10)", "mean(1:10, na.rm = TRUE)")
#> Your call to `mean()` should include `"na.rm"` as one of its arguments. You may have misspelled an argument name, or left out an important argument.

# ...even when the expected argument matches the function's default value
code_feedback("read.csv('file.csv')", "read.csv('file.csv', header = TRUE)")
#> Your call to `read.csv()` should include `"header"` as one of its arguments. You may have misspelled an argument name, or left out an important argument.

# Unstandardized arguments will match by order and value
code_feedback("mean(1:10, 0.1)", "mean(1:10, 0.2)")
#> In `mean(1:10, 0.1)`, I expected `0.2` where you wrote `0.1`.


# give_code_feedback() -------------------------------------------------

# We'll use this example of an incorrect exercise submission throughout
submission_wrong <- mock_this_exercise(
  .user_code = "log(4)",
  .solution_code = "sqrt(4)"
)

# To add feedback to *any* incorrect grade,
# wrap the entire `grade_this()` call in `give_code_feedback()`:
grader <-
  # ```{r example-check}
  give_code_feedback(grade_this({
    pass_if_equal(.solution, "Good job!")
    if (.result < 2) {
      fail("Too low!")
    }
    fail()
  }))
# ```
grader(submission_wrong)
#> <gradethis_graded: [Incorrect]
#>   Too low! I expected you to call `sqrt()` where you called
#>   `log()`.
#> >

# Or you can wrap the message of any fail() directly:
grader <-
  # ```{r example-check}
  grade_this({
    pass_if_equal(.solution, "Good job!")
    if (.result < 2) {
      fail(give_code_feedback("Too low!"))
    }
    fail()
  })
# ```
grader(submission_wrong)
#> <gradethis_graded: [Incorrect]
#>   Too low! I expected you to call `sqrt()` where you called
#>   `log()`.
#> >

# Typically, grade_result() doesn't include code feedback
grader <-
  # ```{r example-check}
  grade_result(
    fail_if(~ round(.result, 0) != 2, "Not quite!")
  )
# ```
grader(submission_wrong)
#> <gradethis_graded: [Incorrect]
#>   Not quite!  Try it again. I have a good feeling about this.
#> >

# But you can use give_code_feedback() to append code feedback
grader <-
  # ```{r example-check}
  give_code_feedback(grade_result(
    fail_if(~ round(.result, 0) != 2, "Not quite!")
  ))
# ```
grader(submission_wrong)
#> <gradethis_graded: [Incorrect]
#>   Not quite!  Don't give up now, try it one more time. I
#>   expected you to call `sqrt()` where you called `log()`.
#> >

# The default `grade_this_code()` `incorrect` message always adds code feedback,
# so be sure to remove \"{maybe_code_feedback()}\" from the incorrect message
grader <-
  # ```{r example-check}
  give_code_feedback(grade_this_code(incorrect = "{random_encouragement()}"))
# ```
grader(submission_wrong)
#> <gradethis_graded_this_code: [Incorrect]
#>   But no need to fret, try it again. I expected you to call
#>   `sqrt()` where you called `log()`.
#> >
```
