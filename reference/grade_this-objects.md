# Checking environment objects for use in `grade_this()`

[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
allows instructors to determine a grade and to create custom feedback
messages using custom R code. To facilitate evaluating the exercise,
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
makes available a number of objects that can be referenced within the
`{ ... }` expression.

All of the objects provided by `learnr` to an exercise checking function
are available for inspection. To avoid name collisions with user or
instructor code, the names of these objects all start with `.`.

- `.label`: The exercise label.

- `.engine`: The exercise engine, typically 'r'.

- `.last_value`: The last value returned from evaluating the user's
  exercise submission.

- `.solution_code`: A string containing the code provided within the
  `*-solution` chunk for the exercise.

- `.user_code`: A string containing the code submitted by the user.

- `.check_code`: A string containing the code provided within the
  `*-check` or `*-code-check` chunk for the exercise.

- `.envir_prep`: A copy of the R environment after running the exercise
  setup code and before the execution of the student's submitted code.

- `.envir_result`: The R environment after running the student's
  submitted code.

- `.envir_solution`: The R environment after running the solution code.

- `.evaluate_result`: The return value from the
  [`evaluate::evaluate()`](https://rstudio.github.io/gradethis/reference/evaluate.r-lib.org/reference/evaluate.md)
  function (see learnr's documentation).

- `.stage`: The current checking stage in the learnr exercise evaluation
  lifecycle: 'code_check', 'error_check', or 'check'

In addition, gradethis has provided some extra objects:

- `.user`, `.result`: The last value returned from evaluating the user's
  exercise submission.

- `.solution`: The last value returned from evaluating the
  `.solution_code` for the exercise (evaluated in `.envir_prep`).

- `.solution_all`: A list containing all solutions when multiple
  solutions are provided in the `*-solution` chunk for the exercise.
  Solutions are separated by header comments, e.g. `# base_r ----`.

- `.solution_code_all`: A list containing the code of all solutions when
  multiple solutions are provided in the `*-solution` chunk for the
  exercise. Solutions are separated by header comments, e.g.
  `# base_r ----`.

## Usage

``` r
.result

.user

.last_value

.solution

.solution_all

.user_code

.solution_code

.solution_code_all

.envir_prep

.envir_result

.envir_solution

.evaluate_result

.label

.stage

.engine
```
