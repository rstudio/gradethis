insert_exercise_grade_code <- function() {
  random_chunk_label <- generate_random_chunk_label()

  # nolint start
  rstudioapi::insertText(glue::glue("
```{r <<random_chunk_label>>, exercise = TRUE}
# student code
____
```

```{r <<random_chunk_label>>-hint-1}
# hint text
\"\"
```

```{r <<random_chunk_label>>-hint-2}
# hint text
\"\"
```

```{r <<random_chunk_label>>-solution}
# solution code

```

```{r <<random_chunk_label>>-check}
# check code
gradethis::grade_code()
```
" , .open = "<<", .close = ">>"))
  # nolint end
}

insert_exercise_grade_result <- function() {
  random_chunk_label <- generate_random_chunk_label()

  # nolint start
  rstudioapi::insertText(glue::glue("
```{r <<random_chunk_label>>, exercise = TRUE}
# student code
____
```

```{r <<random_chunk_label>>-hint-1}
# hint text
\"\"
```

```{r <<random_chunk_label>>-hint-2}
# hint text
\"\"
```

```{r <<random_chunk_label>>-check}
gradethis::grade_result(
  gradethis::pass_if(~ identical(.result, 1), \"YAY!\"),
  gradethis::fail_if(~ identical(.result, 2), \"Try Again.\")
)
```
" , .open = "<<", .close = ">>"))
# nolint end
}

generate_random_chunk_label <- function() {
  paste0(sample(letters, 16, replace = TRUE), collapse = '')
}
