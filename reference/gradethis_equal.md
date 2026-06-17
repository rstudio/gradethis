# Compare the values of two objects to check whether they are equal

Compare the values of two objects to check whether they are equal

## Usage

``` r
gradethis_equal(x = .result, y = .solution, ...)

# Default S3 method
gradethis_equal(x, y, tolerance = sqrt(.Machine$double.eps), ...)

# S3 method for class 'list'
gradethis_equal(x, y, tolerance = sqrt(.Machine$double.eps), ...)
```

## Arguments

- x, y:

  Two objects to compare

- ...:

  Additional arguments passed to methods

- tolerance:

  If non-`NULL`, used as threshold for ignoring small floating point
  difference when comparing numeric vectors. Using any non-`NULL` value
  will cause integer and double vectors to be compared based on their
  values, not their types, and will ignore the difference between `NaN`
  and `NA_real_`.

  It uses the same algorithm as
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html), i.e., first we
  generate `x_diff` and `y_diff` by subsetting `x` and `y` to look only
  locations with differences. Then we check that
  `mean(abs(x_diff - y_diff)) / mean(abs(y_diff))` (or just
  `mean(abs(x_diff - y_diff))` if `y_diff` is small) is less than
  `tolerance`.

## Value

A [logical](https://rdrr.io/r/base/logical.html) value of length one, or
an internal gradethis error.

## Methods (by class)

- `gradethis_equal(default)`: The default comparison method, which uses
  [waldo::compare](https://waldo.r-lib.org/reference/compare.html)

- `gradethis_equal(list)`: The comparison method for lists

## Examples

``` r
gradethis_equal(mtcars[mtcars$cyl == 6, ], mtcars[mtcars$cyl == 6, ])
#> [1] TRUE
gradethis_equal(mtcars[mtcars$cyl == 6, ], mtcars[mtcars$cyl == 4, ])
#> [1] FALSE
```
