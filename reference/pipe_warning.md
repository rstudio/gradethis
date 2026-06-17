# Inform the user about how gradethis interprets piped code

Creates a warning message when user code contains the `%>%`. When
feedback is automatically generated via
[`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
or in
[`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md),
this message attempts to contextualize feedback that might make more
sense when referenced against an un-piped version of the student's code.

## Usage

``` r
pipe_warning(message = getOption("gradethis.pipe_warning"), .user_code = NULL)
```

## Arguments

- message:

  A glue string containing the message. The default value is set with
  the `gradethis.pipe_warning` option.

- .user_code:

  The user's submitted code, found in `env` if `NULL`

## Value

Returns a string containing the pipe warning message, or an empty string
if the `.user_code` does not contain a pipe, if the `.user_code` is also
empty, or if the `message` is `NULL`.

## Options

- `gradethis.pipe_warning`: The default pipe warning message is set via
  this option.

## Glue Variables

The following variables may be used in the glue-able `message`:

- `.user_code`: The student's original submitted code.

- `.user_code_unpiped`: The unpiped version of the student's submitted
  code.

## Examples

``` r
# The default `pipe_warning()` message:
getOption("gradethis.pipe_warning")
#> [1] "I see that you are using pipe operators (e.g. %>%), so I want to let you know that this is how I am interpreting your code before I check it:\n\n```r\n{.user_code_unpiped}\n```\n\n"

# Let's consider two versions of the user code
user_code <-  "penguins %>% pull(year) %>% min(year)"
user_code_unpiped <- "min(pull(penguins, year), year)"

# A `pipe_warning()` is created when the user's code contains `%>%`
pipe_warning(.user_code = user_code)
#> I see that you are using pipe operators (e.g. %>%), so I want to let you know that this is how I am interpreting your code before I check it:
#> 
#> ```r
#> min(pull(penguins, year), year)
#> ```
#> 
#> 

# And no message is created when the user's code in un-piped
pipe_warning(.user_code = user_code_unpiped)
#> [1] ""

# Typically, this warning is only introduced when giving code feedback
# for an incorrect submission. Here we didn't expect `year` in `min()`.
submission <- mock_this_exercise(
  .user_code = !!user_code,
  .solution_code = "penguins %>% pull(year) %>% min()"
)

grade_this_code()(submission)
#> <gradethis_graded_this_code: [Incorrect]
#>   I see that you are using pipe operators (e.g. %>%), so I
#>   want to let you know that this is how I am interpreting your
#>   code before I check it:
#> 
#>   ```r
#>   min(pull(penguins, year), year)
#>   ```
#> 
#>   I did not expect your call to `min()` to include `year`. You
#>   may have included an unnecessary argument, or you may have
#>   left out or misspelled an important argument name. Try it
#>   again; next time's the charm!
#> >
```
