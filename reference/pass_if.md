# Signal a passing or failing grade if a condition is TRUE

`pass_if()` and `fail_if()` both create passing or failing grades if a
given condition is `TRUE`. See
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
for more information on gradethis grade-signaling functions.

These functions are also used in legacy gradethis code, in particular in
the superseded function
[`grade_result()`](https://rstudio.github.io/gradethis/reference/grade_result.md).
While previous versions of gradethis allowed the condition to be
determined by a function or formula, when used in
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
the condition must be a logical `TRUE` or `FALSE`.

## Usage

``` r
pass_if(
  cond,
  message = NULL,
  ...,
  env = parent.frame(),
  praise = getOption("gradethis.pass.praise", FALSE),
  x = deprecated()
)

fail_if(
  cond,
  message = NULL,
  ...,
  env = parent.frame(),
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE),
  x = deprecated()
)
```

## Arguments

- cond:

  A logical value or an expression that will evaluate to a `TRUE` or
  `FALSE` value. If the value is `TRUE`, or would be considered `TRUE`
  in an `if (cond)` statement, then a passing or failing grade is
  returned to the user.

- message:

  A character string of the message to be displayed. In all grading
  helper functions other than
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md),
  `message` is a template string that will be processed with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- ...:

  Passed to
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  in
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md).

- env:

  environment to evaluate the glue `message`. Most users of gradethis
  will not need to use this argument.

- praise:

  Include a random praising phrase with
  [`random_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)?
  The default value of `praise` can be set using
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  or the `gradethis.pass.praise` option.

- x:

  Deprecated. Replaced with `cond`.

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

`pass_if()` and `fail_if()` signal a correct or incorrect grade if the
provided condition is `TRUE`.

## Functions

- `pass_if()`: Pass if `cond` is `TRUE`.

- `fail_if()`: Fail if `cond` is `TRUE`.

## See also

Other grading helper functions:
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md),
`pass_if()`, `fail_if()`,
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
[`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md).

## Examples

``` r
# Suppose the prompt is to find landmasses in `islands` with land area of
# less than 20,000 square miles. (`islands` reports land mass in units of
# 10,000 sq. miles.)

grader <-
  # ```{r example-check}
  grade_this({
    fail_if(any(is.na(.result)), "You shouldn't have missing values.")

    diff_len <- length(.result) - length(.solution)
    fail_if(diff_len < 0, "You missed {abs(diff_len)} island(s).")
    fail_if(diff_len > 0, "You included {diff_len} too many islands.")

    pass_if(all(.result < 20), "Great work!")

    # Fall back grade
    fail()
  })
# ```

.solution <-
  # ```{r example-solution}
  islands[islands < 20]
# ```

# Peek at the right answer
.solution
#>     Axel Heiberg           Hainan           Kyushu         Melville 
#>               16               13               14               16 
#>      New Britain  Prince of Wales      Southampton      Spitsbergen 
#>               15               13               16               15 
#>           Taiwan Tierra del Fuego            Timor        Vancouver 
#>               14               19               13               12 

# Has missing values somehow
grader(mock_this_exercise(islands["foo"], !!.solution))
#> <gradethis_graded: [Incorrect]
#>   You shouldn't have missing values.
#> >

# Has too many islands
grader(mock_this_exercise(islands[islands < 29], !!.solution))
#> <gradethis_graded: [Incorrect]
#>   You included 4 too many islands.
#> >

# Has too few islands
grader(mock_this_exercise(islands[islands < 16], !!.solution))
#> <gradethis_graded: [Incorrect] You missed 4 island(s).>

# Just right!
grader(mock_this_exercise(islands[islands < 20], !!.solution))
#> <gradethis_graded: [Correct] Great work!>
```
