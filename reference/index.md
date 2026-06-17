# Package index

## Setup gradethis

To use gradethis in your learnr tutorial, you only need to load the
gradethis package. Still, you can use
[`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
to change the default values of many of the exercise checking or grading
function arguments.

- [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  : Setup gradethis for use within learnr

## Exercise Checking Functions

Choose one of these functions to use in the `*-check` chunk of your
exercise.

- [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  : Grade a student's submission using custom logic

- [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
  : Grade student code against a solution

- [`debug_this()`](https://rstudio.github.io/gradethis/reference/debug_this.md)
  : Debug an exercise submission

- [`.result`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.user`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.last_value`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.solution`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.solution_all`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.user_code`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.solution_code`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.solution_code_all`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.envir_prep`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.envir_result`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.envir_solution`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.evaluate_result`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.label`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.stage`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  [`.engine`](https://rstudio.github.io/gradethis/reference/grade_this-objects.md)
  :

  Checking environment objects for use in
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)

## Signal a Final Grade

Helper functions to create and signal a final grade when used in custom
checking logic in
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md).

- [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md)
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md) :
  Signal a final grade for a student's submission
- [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  [`fail_if_not_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  : Signal a passing or failing grade if two values are equal
- [`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  [`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  : Signal a passing or failing grade if a condition is TRUE
- [`fail_if_code_feedback()`](https://rstudio.github.io/gradethis/reference/fail_if_code_feedback.md)
  : Signal a failing grade if mistakes are detected in the submitted
  code
- [`fail_if_error()`](https://rstudio.github.io/gradethis/reference/fail_if_error.md)
  : Fail if grading code produces an error

## Generate Feedback Messages

Create feedback messages for use in
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
grades or in the exercise checking functions.

- [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  [`give_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  : Provide automated code feedback
- [`random_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)
  [`random_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md)
  [`give_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)
  [`give_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md)
  : Random praise and encouragement
- [`pipe_warning()`](https://rstudio.github.io/gradethis/reference/pipe_warning.md)
  : Inform the user about how gradethis interprets piped code

## Interact with Objects

These functions help you access objects created by student and solution
code.

- [`user_object_get()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  [`solution_object_get()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  [`user_object_exists()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  [`solution_object_exists()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  [`user_object_list()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  [`solution_object_list()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  : Functions for interacting with objects created by student and
  solution code

## Helper Functions and Utilities

These functions are used by gradethis to work within learnr tutorials,
or are available to help tutorial authors test their grading logic. Most
users of gradethis will not need to use these functions.

- [`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md)
  :

  A checker function to use with learnr

- [`gradethis_error_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_error_checker.md)
  : An error checking function for use with learnr

- [`mock_this_exercise()`](https://rstudio.github.io/gradethis/reference/mock_this_exercise.md)
  : Mock a user submission to an exercise

- [`with_exercise()`](https://rstudio.github.io/gradethis/reference/with_exercise.md)
  :

  Run an expression as if it were in an exercise's
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  block

- [`gradethis_equal()`](https://rstudio.github.io/gradethis/reference/gradethis_equal.md)
  : Compare the values of two objects to check whether they are equal
