# Random praise and encouragement

Generate a random praise or encouragement phrase. These functions are
designed for use within
[`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) or
[`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
messages, or anywhere else that gradethis provides feedback to the
student.

## Usage

``` r
random_praise()

random_encouragement()

give_praise(expr, ..., location = "before", before = NULL, after = NULL)

give_encouragement(expr, ..., location = "after", before = NULL, after = NULL)
```

## Arguments

- expr:

  A
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  grade or helper function, or a grading function — like
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  or
  [`grade_result()`](https://rstudio.github.io/gradethis/reference/grade_result.md)
  — or a character string. Praise will be added to any passing grades
  and encouragement will be added to any failing grade. If `expr` is a
  character string, then `"{random_praise()}"` or
  `"{random_encouragement()}"` is pasted before or after the string
  according to `location`.

- ...:

  Ignored.

- location:

  Should the praise or encouragement be added before or after the grade
  message?

- before, after:

  Text to be added before or after the praise or encouragement phrase.

## Value

- `random_praise()` and `random_encouragement()` each return a
  length-one string with a praising or encouraging phrase.

- `give_praise()` and `give_encouragement()` add praise or encouragement
  phrases to passing and failing grades, respectively.

## Functions

- `random_praise()`: Random praising phrase

- `random_encouragement()`: Random encouraging phrase

- `give_praise()`: Add praising message to a passing grade.

- `give_encouragement()`: Add encouraging message to a failing grade.

## Examples

``` r
replicate(5, glue::glue("Random praise: {random_praise()}"))
#> [1] "Random praise: Success!"            
#> [2] "Random praise: Well done!"          
#> [3] "Random praise: Absolutely fabulous!"
#> [4] "Random praise: Cool job!"           
#> [5] "Random praise: Wonderful!"          
replicate(5, glue::glue("Random encouragement: {random_encouragement()}"))
#> [1] "Random encouragement: Let's try it again."                              
#> [2] "Random encouragement: Please try again."                                
#> [3] "Random encouragement: Try it again. Perseverance is the key to success."
#> [4] "Random encouragement: Give it another try."                             
#> [5] "Random encouragement: Don't give up now, try it one more time."         

# give_praise() adds praise to passing grade messages
give_praise(pass("That's absolutely correct."))
#> <gradethis_graded: [Correct]
#>   Excellent! That's absolutely correct.
#> >

# give_encouragement() encouragement to failing grade messages
give_encouragement(fail("Sorry, but no."))
#> <gradethis_graded: [Incorrect]
#>   Sorry, but no. Please try again.
#> >
```
