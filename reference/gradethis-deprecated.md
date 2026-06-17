# Deprecated

Deprecated

## Usage

``` r
grade_feedback(...)

grade_conditions(
  ...,
  correct = NULL,
  incorrect = NULL,
  grader_args = deprecated(),
  learnr_args = deprecated(),
  glue_correct = getOption("gradethis.glue_correct_test"),
  glue_incorrect = getOption("gradethis.glue_incorrect_test")
)

random_encourage()

grade_learnr(...)
```

## Functions

- `grade_feedback()`: **\[deprecated\]** Removed from package

- `grade_conditions()`: **\[superseded\]** Use
  [`grade_result_strict()`](https://rstudio.github.io/gradethis/reference/grade_result.md)

- `random_encourage()`: **\[superseded\]** Use
  [`random_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md).

- `grade_learnr()`: **\[superseded\]** Use
  [`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md).
