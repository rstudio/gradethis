# Grading Demo

If you are using the RStudio IDE, `gradethis_demo()` opens an example
learnr file that demonstrates how to use the grader package to check
student code.

## Usage

``` r
gradethis_demo()
```

## Details

The tutorial sets the learnr `exercise.checker` option to
[`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md)
in the document's setup chunk.

It then uses three different exercise checking methods:
[`grade_result()`](https://rstudio.github.io/gradethis/reference/grade_result.md),
[`grade_conditions()`](https://rstudio.github.io/gradethis/reference/gradethis-deprecated.md),
and
[`grade_code()`](https://rstudio.github.io/gradethis/reference/grade_code.md).

To use a checking method, follow the exercise chunk with a chunk whose
label matches the label of the exercise chunk (ex: `myexercise`) but
includes the suffix `-check` (ex: `myexercise-check`). Call any checking
method in that chunk.

To ensure that checking method can provide informative feedback, you may
provide custom `correct` and `incorrect` messages.

If you are not using RStudio IDE, you can access the demo file at
`system.file("extdata", "grading-demo/grading-demo.Rmd", package = "grader")`.
