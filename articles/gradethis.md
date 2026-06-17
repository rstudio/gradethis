# gradethis

## Overview

gradethis helps instructors provide automated feedback for interactive
exercises in [learnr](https://rstudio.github.io/learnr/) tutorials. If
you are new to writing learnr tutorials, we recommend that you get
comfortable with the [learnr tutorial
format](https://rstudio.github.io/learnr/) before incorporating
gradethis.

gradethis provides two grading modalities. You can:

1.  Compare student code to model solution code with
    [`grade_this_code()`](#compare-with-solution-code), or

2.  Write custom grading logic with
    [`grade_this()`](#custom-grading-logic).

To use gradethis in a learnr tutorial, load gradethis after learnr in
the `setup` chunk of your tutorial:

```` markdown
```{r setup}
library(learnr)
library(gradethis)
```
````

To help authors provide consistent feedback, gradethis uses global
options to control the default behavior of many of its functions. You
can set or change these default values using
[`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).

## Checking Student Code

Suppose we ask our students to calculate the average of the first ten
integers. In our tutorial’s R Markdown, we include a prompt, followed by
an `exercise` chunk.

```` markdown
**Calculate the average of all of the integers from 1 to 10.**

```{r average, exercise = TRUE}
____(1:10)
```
````

Our hope is that students will replace the `____` with the correct R
function, in this case [`mean()`](https://rdrr.io/r/base/mean.html).

To grade an exercise, we need to associate checking code with the
exercise. In learnr, this code is written in a chunk named
`<exercise label>-check`, where `<exercise label>` is the label of the
chunk with `exercise = TRUE`.

To use gradethis to grade our `average` exercise, we create a new chunk
named `average-check` and we call either
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
or
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
inside the chunk.

### Custom Grading Logic

In the `average-check` chunk, we can either call
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
or
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md).
The first gives authors control over the grading logic, which is
typically written inside curly braces as the first argument of
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md):

```` markdown
**Calculate the average of all of the integers from 1 to 10.**

```{r average, exercise = TRUE}
____(1:10)
```

```{r average-check}
grade_this({
  # custom grading code
})
```
````

We’ll cover custom grading logic in more detail below.

### Compare with Solution Code

On the other hand,
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
requires authors to include a model solution to which the student’s
submission will be compared. The solution is written in the
`<exercise label>-solution` chunk, while
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
is called in the `<exercise label>-check` chunk:

```` markdown
**Calculate the average of all of the integers from 1 to 10.**

```{r average, exercise = TRUE}
____(1:10)
```

```{r average-solution}
mean(1:10)
```

```{r average-check}
grade_this_code()
```
````

When a student submits their exercise solution, it is automatically
compared to the model solution in the `-solution` chunk, and the first
encountered difference is reported to the student. If there are no
differences, a positive statement is returned to the student.

Beautiful! Correct!

If the student submits an incorrect answer, such as `max(1:10)`, the
feedback message attempts to point the student in the right direction.

I expected you to call [`mean()`](https://rdrr.io/r/base/mean.html)
where you called [`max()`](https://rdrr.io/r/base/Extremes.html). Try it
again. I have a good feeling about this.

## Writing Custom Logic

If you want to provide custom feedback for specific errors that students
may be likely to make in your exercise,
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
provides a high level of flexibility.

When using
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
your goal is to write a series of tests around the student’s submission
to determine whether or not it passes or fails. A passing or failing
grade is signaled with
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) or
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md). You
may provide a custom message or rely on the gradethis default passing or
failing message.

In the case of our `average` exercise, a simple version of exercise
grading might check that the student’s `.result` is equal to
`mean(1:10)`. If it is, the grading code returns a passing grade,
otherwise a failing grade is returned.

```` markdown
```{r average-check}
grade_this({
  if (identical(.result, mean(1:10))) {
    pass("Great work! You calculated the average of the first ten integers.")
  }
  fail()
})
```
````

If a hypothetical student submitted `mean(1:10)` for this exercise, our
checking code will return:

Great work! You calculated the average of the first ten integers.

If the student submits an incorrect answer, the default
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
message is encouraging and helpful:

Incorrect. I expected you to call
[`mean()`](https://rdrr.io/r/base/mean.html) where you called
[`min()`](https://rdrr.io/r/base/Extremes.html). Give it another try.

This example highlights three important aspects of custom grading logic
that we will cover in more detail:

1.  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
    makes available a [set of objects](#exercise-objects) related to the
    exercise and the student’s submission, such as `.result`.

2.  [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md)
    and
    [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
    signal a final grade *as soon as they are called*. There are also
    [additional grade-signaling functions](#grading-helper-functions)
    for common scenarios.

3.  The default
    [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
    message includes code feedback, if a solution is available. Code
    feedback, encouragement, and praise can be [enabled or
    disabled](#feedback-messages) for
    [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md)
    and
    [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
    grades directly or via global options.

### Exercise Objects

The checking code you write inside
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
will be executed in an environment where a number of submission- and
exercise-specific objects have been added.

Among these objects,
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
includes all of the objects provided by `learnr` to an exercise checking
function. For convenience, gradethis also includes a few additional
objects. To avoid name collisions with user or instructor code, the
names of these objects all start with `.`:

| Object | Description |
|:---|:---|
| `.label` | Label for exercise chunk |
| `.solution_code` | Code provided within the `*-solution` chunk for the exercise |
| `.user_code` | R code submitted by the user |
| `.last_value` | The last value from evaluating the user’s exercise submission |
| `.result`, `.user` | A direct copy of `.last_value` for friendlier naming |
| `.solution` | When accessed, will be the result of evaluating the `.solution_code` in a child environment of `.envir_prep` |
| `.check_code` | Code provided within the `*-check` (or `*-code-check`) chunk for the exercise |
| `.envir_prep` | A copy of the R environment before the execution of the chunk |
| `.envir_result` | The R environment after the execution of the chunk |
| `.evaluate_result` | The return value from the [`evaluate::evaluate`](https://rstudio.github.io/gradethis/articles/evaluate.r-lib.org/reference/evaluate.md) function |

You can reference any of these objects in your grading logic.
Additionally, you can use
[`debug_this()`](https://rstudio.github.io/gradethis/reference/debug_this.md)
as your exercise checker or in lieu of a grade to return an informative
message in the tutorial for your visual inspection of the value of each
of these objects. This is useful when debugging or writing exercises.

### Grading Helper Functions

There are a number of grading helper functions in addition to
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) and
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md):

- [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  and
  [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  compare the submitted `.result` (or another object) to an expected
  value and pass or fail if the two values are equal.

- [`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  and
  [`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  pass or fail if their first argument, a condition, is `TRUE`.

- [`fail_if_code_feedback()`](https://rstudio.github.io/gradethis/reference/fail_if_code_feedback.md)
  fails if there are differences between the submitted code and the
  model solution code. This is useful when you want to use custom logic
  to check specific failure modes, but want to fall back to code
  comparison for modes you may not have anticipated.

Each of these functions signals a final grade as soon as they are
called. This allows tutorial authors to construct grading logic in such
a way that later tests presume earlier tests in the grading code were
not triggered. If you are familiar with writing R functions, the
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) and
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
helper functions are similar to early
[`return()`](https://rdrr.io/r/base/function.html) statements.

Be careful to end your
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
grading code with a final fallback grade in case no other grades are
returned. Typically, this fallback grade will be a call to
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) or
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
without any arguments.

### Feedback Messages

The [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md)
and [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
functions all use the [glue package](https://glue.tidyverse.org) for
custom message templating. With a
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
template string, you can easily interleave R values with the feedback.

``` r

fail(
  "Your code returned {round(.result, 2)}, but the average of `1:10` is 
  {if (.result > .solution) 'lower' else 'higher'} than that value."
)
```

Your code returned 5.5, but the average of `1:10` is higher than that
value.

Notice that you may include R code wrapped in
[`{ }`](https://rdrr.io/r/base/Paren.html) inside the template string
and you may reference any of the [exercise objects](#exercise-objects)
in that string.

There are a number of message components that you may want to
consistently include in your feedback:

- [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md)
  includes an additional `praise` argument to prepend
  [`random_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)
  to the feedback

- [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  includes both `hint` and `encouarge`. When `TRUE`, `hint` adds code
  feedback to the failing message (if a solution is available) and
  `encourage` appends
  [`random_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md)
  to the end of the feedback message.

``` r

pass("That's exactly the average of 1 through 10.")
```

That's exactly the average of 1 through 10.

``` r

pass("That's exactly the average of 1 through 10.", praise = TRUE)
```

Magnificent! That's exactly the average of 1 through 10.

Each of the `praise`, `encourage`, and `hint` arguments can be enabled
or disabled for all
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) or
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
grades via the global options set by
[`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).

``` r

gradethis_setup(
  pass.praise = TRUE,
  fail.encourage = TRUE,
  fail.hint = TRUE
)
```

``` r

pass("That's exactly the average of 1 through 10.")
```

Magnificent! That's exactly the average of 1 through 10.

``` r

fail("Your answer was not the number I expected.")
```

Your answer was not the number I expected. I expected you to call
[`mean()`](https://rdrr.io/r/base/mean.html) where you called
[`min()`](https://rdrr.io/r/base/Extremes.html). Don't give up now, try
it one more time.

Notice that by globally turning on `praise`, `encourage`, and `hint`,
custom feedback messages will contain the same components as the default
pass and fail messages. (You can control those defaults with the `pass`
and `fail` arguments of
[`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).)
Without those options enabled globally, custom messages will only
contain the text included in the message.
