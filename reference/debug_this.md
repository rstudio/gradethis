# Debug an exercise submission

When used in a `*-check` chunk or inside
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
`debug_this()` displays in the learnr tutorial a complete listing of the
variables and environment available for checking. This can be helpful
when you need to debug an exercise and a submission.

## Usage

``` r
debug_this(check_env = parent.frame())
```

## Arguments

- check_env:

  A grade checking environment. You can use
  [`mock_this_exercise()`](https://rstudio.github.io/gradethis/reference/mock_this_exercise.md)
  to prepare a mocked exercise submission environment. Otherwise, you
  don't need to use or set this argument.

## Value

Returns a neutral grade containing a message that includes any and all
information available about the exercise and the current submission. The
output lets you visually explore the objects available for use within
your
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
grading code.

## Debugging exercises

`debug_this()` gives you a few ways to see the objects that are
available inside
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
for you to use when grading exercise submissions. Suppose we have this
example exercise:

    ```{r example-setup}
    x <- 1
    ```

    ```{r example, exercise = TRUE}
    # user submits
    y <- 2
    x + y
    ```

    ```{r example-solution}
    x + 3
    ```

The debug output will look like the following when used as described
below.

> **Exercise label (`.label`):** `example`  
> **Engine (`.engine`):** `r`
>
> Submission (`.result`, `.user`, `.last_value`):
>
> ``` r
> [1] 3
> ```
>
> Solution (`.solution`):
>
> ``` r
> [1] 4
> ```
>
> `.envir_prep`
>
>      $ x: num 1
>
> `.envir_result`
>
>      $ x: num 1
>
> `.envir_solution`
>
>      $ x: num 1
>
> `.user_code`
>
> ``` r
> # user submits
> x + 2
> ```
>
> `.solution_code`
>
> ``` r
> x + 3
> ```

### Always debug

The first method is the most straight-forward. Inside the `*-check` or
`*-error-check` chunks for your exercise, simply call `debug_this()`:

    ```{r example-check}
    debug_this()
    ```

Every time you submit code for feedback via **Submit Answer**, the debug
information will be printed.

### Debug specific cases

On the other hand, if you want to debug a specific submission, such as a
case where a submission isn't matching any of your current grading
conditions, you can call `debug_this()` wherever you like inside
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md).

    ```{r example-check}
    grade_this({
      pass_if_equal(3, "Good work?")

      # debug the submission if it is somehow equal to 2
      if (.result == 2) {
        debug_this()
      }
    })
    ```

### Debug default fail condition

It's common to have the grade-checking code default to an incorrect
grade with code feedback by calling
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md) at
the end of the checking code in
[`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md).
During development of a tutorial, you may want to have this default
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
return the debugging information rather than a failure.

By setting the global option `gradethis.fail` to use `debug_this()`,

    ```{r setup}
    library(learnr)
    library(gradethis)
    gradethis_setup()

    option(gradethis.fail = "{debug_this()}")
    ```

you can see the values that are available to you during the submission
check whenever your test submissions pass through your other checks.

    ```{r example-check}
    grade_this({
      pass_if_equal(3, "Good work?")

      fail()
    })
    ```

Don't forget to reset or unset the `gradethis.fail` option when you're
done working on your tutorial.

## Examples

``` r
# Suppose we have an exercise (guess the number 42). Mock a submission:
submission <- mock_this_exercise(.user_code = 40, .solution_code = 11 + 31)

# Call `debug_this()` inside your *-check chunk, is equivalent to
debug_this()(submission)$message
#> <p>
#>   <strong>Exercise label (<code>.label</code>):</strong>
#>   <code>mock</code>
#>   <br/>
#>   <strong>Engine (<code>.engine</code>):</strong>
#>   <code>r</code>
#> </p>
#> <p>
#>   Submission (<code>.result</code>, <code>.user</code>, <code>.last_value</code>):
#>   <pre class="r"><code>[1] 40</code></pre>
#> </p>
#> <p>
#>   Solution (<code>.solution</code>):
#>   <pre class="r"><code>[1] 42</code></pre>
#> </p>
#> <details>
#>   <summary>
#>     <code>.envir_prep</code>
#>   </summary>
#>   <pre><code></code></pre>
#> </details>
#> <details>
#>   <summary>
#>     <code>.envir_result</code>
#>   </summary>
#>   <pre><code></code></pre>
#> </details>
#> <details>
#>   <summary>
#>     <code>.envir_solution</code>
#>   </summary>
#>   <pre><code></code></pre>
#> </details>
#> <details>
#>   <summary>
#>     <code>.user_code</code>
#>   </summary>
#>   <pre class="r"><code>40</code></pre>
#> </details>
#> <details>
#>   <summary>
#>     <code>.solution_code</code>
#>   </summary>
#>   <pre class="r"><code>11 + 31</code></pre>
#> </details>

# The remaining examples produce equivalent output
if (FALSE) { # \dontrun{
# Or you can call `debug_this()` inside a `grade_this()` call
# at the point where you want to get debug feedback.
grade_this({
  pass_if_equal(42, "Good stuff!")

  # Find out why this is failing??
  debug_this()
})(submission)

# Set default `fail()` message to show debug information
# (for tutorial development only!)
old_opts <- options(gradethis.fail = "{debug_this()}")

grade_this({
  pass_if_equal(42, "Good stuff!")

  fail()
})(submission)

# default fail() will show debug until you reset gradethis.fail option
options(old_opts)
} # }
```
