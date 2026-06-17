# Evaluates a condition

**\[superseded\]** Please use
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
mixed with
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md),
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md),
and/or
[`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md).
Can also use
[`eval_gradethis()`](https://rstudio.github.io/gradethis/reference/eval_gradethis.md).

Evaluates the
[`condition()`](https://rstudio.github.io/gradethis/reference/grade_result.md)
object to return a
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
value.

## Usage

``` r
evaluate_condition(condition, ..., last_value, env)
```

## Arguments

- condition:

  a
  [`condition()`](https://rstudio.github.io/gradethis/reference/grade_result.md)
  object

- ...:

  ignored

- last_value:

  The last value from evaluating the user's exercise submission.

- env:

  environment to evaluate the condition

## Value

a [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
value if `condi$x` is `TRUE` or `NULL` if `condi$x` is `FALSE`

## Examples

``` r
condi_formula_t <- condition(
  ~ identical(.result, 5),
  message = "my correct message",
  correct = TRUE
)
evaluate_condition(
  condi_formula_t,
  last_value = 5,
  env = new.env()
)
#> <gradethis_graded: [Correct] my correct message>
```
