# Multiple solutions

## Overview

Not all exercises have exactly one right answer: there could be a few
different ways to arrive at the same result, or the exercise prompt
could be open-ended and ask the student to pick from a selection of
possibilities. In either case, and with minimal changes to your learnr
tutorial, gradethis can accommodate exercises with multiple solutions!

To signal that your exercise has multiple solutions, write all of the
solutions in the exercise’s `-solution` chunk, separating the solutions
with **code headers** — a comment followed by at least four dashes.

    # code header ----

You can think of the code headers as *labels* for each solution variant.
Here’s an example `-solution` chunk for an exercise with two possible
solutions (the [full exercise](#example-different-results) is presented
below).

```` markdown
```{r average-solution}
# mean ----
mean(fibonacci)

# median ----
median(fibonacci)
```
````

There are two types of exercises with multiple solutions:

1.  **Multiple solutions, same result.** When the solutions describe
    multiple ways of reaching the same final result, gradethis provides
    code feedback hints based on the closest solution to the user’s
    code.

    In most cases, you’ll only need to write out the solutions in the
    `-solution` chunk, and gradethis will take care of the rest. Learn
    more in [Multiple solutions, same result](#same-result) below.

2.  **Multiple solutions, different results.** Other exercises have more
    than one acceptable result. In these exercises, multiple solutions
    can be used both for grading students’ results and for providing
    code feedback.

    In addition to writing out the solutions in the `-solution` chunk,
    you may also need to modify your grading code slightly. Learn more
    in [Multiple solutions, different results](#different-results)
    below.

In both cases, multiple solutions are written in the same way in the
`-solution` chunk, and minimal changes are needed in your grading code.
For advanced grading requirements, gradethis adds two objects into the
grading environment: [`.solution_code_all`](#solution-code-all) and
[`.solution_all`](#solution-all). We’ll cover both straight-forward and
advanced uses in the examples that follow.

## Multiple solutions, same result

If all your solutions have the same result, you might not need to do
anything special in your grading code, as we’ll see in the next example.

### Example

In this example, students are asked to convert a numeric value to a
whole number. The most likely solution is to use the
[`round()`](https://rdrr.io/r/base/Round.html) function, but a creative
student might call [`floor()`](https://rdrr.io/r/base/Round.html) or
[`as.integer()`](https://rdrr.io/r/base/integer.html).

Notice that we include all three variations in the `round-solution`
chunk below, but we’ve placed the answer we consider “most correct” as
the last solution in the chunk. The last solution in the chunk will be
the default solution used by gradethis when the student’s code is not a
good match for any of the solutions.

```` markdown
```{r setup}
library(learnr)
library(gradethis)
```

Convert 5.2 to a reasonable whole number.

```{r round, exercise = TRUE}
____(5.2)
```

```{r round-solution}
# floor ----
floor(5.2)

# as.integer ----
as.integer(5.2)

# round ----
round(5.2)
```

```{r round-check}
grade_this({
  # provide code hints if submission is wrong
  fail_if_code_feedback()

  # pass if the value is correct
  pass_if_equal()

  # or fallback to a failing grade
  fail(hint = TRUE)
})
```
````

If a student enters any of the correct answers to arrive at the value
`5`, they receive positive feedback and encouragement.

``` r

as.integer(5.2)
```

Fantastic! Correct!

If a student gives an incorrect answer using
[`round()`](https://rdrr.io/r/base/Round.html), the code feedback hint
provided by `fail(hint = TRUE)` will nudge them towards the correct
[`round()`](https://rdrr.io/r/base/Round.html) solution:

``` r

round(5.2, digits = 1)
```

I did not expect your call to
[`round()`](https://rdrr.io/r/base/Round.html) to include `digits = 1`.
You may have included an unnecessary argument, or you may have left out
or misspelled an important argument name.

And if a student gives an incorrect answer but their text is closer to
one of the other solutions, the code feedback will nudge the student
toward the correct solution.

``` r

flor(5.2)
```

I expected you to call [`floor()`](https://rdrr.io/r/base/Round.html)
where you called `flor()`.

``` r

as.numeric(5.2)
```

I expected you to call
[`as.integer()`](https://rdrr.io/r/base/integer.html) where you called
[`as.numeric()`](https://rdrr.io/r/base/numeric.html).

### Providing different messages for different solutions

Grading functions that use solution code — like
[`fail_if_code_feedback()`](https://rstudio.github.io/gradethis/reference/fail_if_code_feedback.md),
[`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
or `fail(hint = TRUE)` — will automatically give feedback based on the
closest solution to the student’s input. In some cases, you may want to
accept multiple solutions, but give different feedback depending on the
solution the student gave.

To enable advanced custom grading, gradethis makes all of the solutions
to an exercise available to grading code as `.solution_code_all`, a
named list containing the code for each solution.

You can use code like the following template to give a special message
(`<message>`) for user input that matches a particular solution
identified by the label used for the code heading above the solution
(`<solution_label>`):

``` r

correctly_used_solution <-
  is.null(code_feedback(.user_code, .solution_code_all[["<solution_label>"]]))

pass_if(
  correctly_used_solution,
  message = "<message>"
)
```

Some things of note in this template:

1.  You can obtain the code for a specific solution using the label in
    its code heading: e.g. `.solution_code_all[["floor"]]` in our
    example would return `"floor(5.2)"`.

2.  You need to explicitly ask for
    [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
    for the solution of interest. Otherwise, by default,
    [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
    will first match the user’s code with the most option among the set
    of solutions.

3.  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
    returns `NULL` when `user_code` matches `solution_code`. Testing
    that the feedback [`is.null()`](https://rdrr.io/r/base/NULL.html) is
    a robust way to test that the student submitted code that matches a
    solution because it ignores minor differences like whitespace and
    pipe usage.

#### Example

Using the template above, we can provide some feedback specifically to
students who use the [`floor()`](https://rdrr.io/r/base/Round.html)
solution.

```` markdown
```{r round-check}
grade_this({
  correctly_used_floor <-
    is.null(code_feedback(.user_code, .solution_code_all[["floor"]]))

  pass_if(
    correctly_used_floor,
    "Correct, but remember that `floor()` always rounds down.
    If you want to round to the nearest whole number, `round()` is usually safer."
  )
  pass_if_equal()
  fail(hint = TRUE)
})
```
````

``` r

floor(5.2)
```

Correct, but remember that
[`floor()`](https://rdrr.io/r/base/Round.html) always rounds down. If
you want to round to the closest whole number,
[`round()`](https://rdrr.io/r/base/Round.html) is usually safer.

## Multiple solutions, different results

If your exercise has multiple acceptable results, you’ll need to
slightly adapt your code to ensure students’ inputs are tested against
all acceptable results.

To compare the result of student input to each solution, you should use
`.solution_all` as the first argument in
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md).
Multiple solution results are saved in an environment called
`.solution_all`. (Because `.solution_all` is an environment, it requires
some special handling. Check [Working with
`.solution_all`](#solution-all) for more details.)

Within
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
messages, you can use the object `.solution_label`, which corresponds to
the code header of the solution that the student’s code matched.

Functions that grade students’ code can still be used without any
changes.

### Example

```` markdown
```{r setup}
library(learnr)
library(gradethis)
fibonacci <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
```

Use `mean()` or `median()` to find the average of `fibonacci`, a vector containing the first 10 numbers in the Fibonacci sequence.

```{r average, exercise = TRUE}
____(fibonacci)
```

```{r average-solution}
# mean ----
mean(fibonacci)

# median ----
median(fibonacci)
```

```{r average-check}
grade_this({
  pass_if_equal(.solution_all, "You solved it with `{.solution_label}()`!")
  fail(hint = TRUE)
})
```
````

If a student gives either correct solution,
[`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
will give them a passing grade:

``` r

mean(fibonacci)
```

You solved it with [`mean()`](https://rdrr.io/r/base/mean.html)!

``` r

median(fibonacci)
```

You solved it with [`median()`](https://rdrr.io/r/stats/median.html)!

If the student’s input doesn’t match any of the solutions, grading will
proceed as normal:

``` r

sum(fibonacci)
```

I expected you to call [`floor()`](https://rdrr.io/r/base/Round.html)
where you called [`sum()`](https://rdrr.io/r/base/sum.html).

### Working with all solution results

`.solution_all` is an environment, not a list. This carries certain
advantages. Exercises can be graded faster because each solution can be
evaluated as needed rather than all up front.

But it also comes with drawbacks. The most important are:

1.  environments can’t be subset with numbers, only names, and
2.  all elements of an environment need to have a unique name.

If you need to access the results of specific solutions, make sure you
give a unique label to every solution.

``` r

# mean ----
mean(fibonacci)

# median ----
median(fibonacci)
```

To reference a specific solution result in grading code, use the
solution label to access the result in the `.solution_all`:

``` r

grade_this({
  pass_if_equal(
    .solution_all[["mean"]],
    "That's right! Do you think the mean is skewed?"
  )
  pass_if_equal(.solution_all)
  fail_if_code_feedback()
  fail()
})
```

If you don’t give your solutions unique names, `gradethis` will generate
unique names for you. For example, these solutions

``` r

# mean ----
mean(fibonacci)

# mean ----
mean(fibonacci, na.rm = TRUE)

# median ----
median(fibonacci)

# median ----
median(fibonacci, na.rm = TRUE)
```

have these names:

    #> [1] "mean"     "mean_1"   "median"   "median_1"

This means that if you need to access specific solution results in your
grading code, it’s almost always better to give your solutions unique
labels. On the other hand, if your passing message makes use of
`.solution_label`, you may want to duplicate the solution labels.

Recall that the `-check` chunk for the `average` exercise was:

```` markdown
```{r average-check}
grade_this({
  pass_if_equal(.solution_all, "You solved it with `{.solution_label}()`!")
  fail(hint = TRUE)
})
```
````

By including the [`mean()`](https://rdrr.io/r/base/mean.html) and
[`median()`](https://rdrr.io/r/stats/median.html) solution variants with
`na.rm = TRUE` we get an appropriate passing message even though the
solution labels are duplicated:

``` r

median(fibonacci, na.rm = TRUE)
```

You solved it with [`median()`](https://rdrr.io/r/stats/median.html)!
