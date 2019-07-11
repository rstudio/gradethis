insert_exercise_check_code <- function() {
  random_chunk_label <- generate_random_chunk_label()

  rstudioapi::insertText(glue::glue("
```{r <<random_chunk_label>>, exercise = TRUE}
# student code

```

```{r <<random_chunk_label>>-hint-1}
# hint text

```

```{r <<random_chunk_label>>-hint-2}
# hint text

```

```{r <<random_chunk_label>>-solution}
# solution code

```

```{r <<random_chunk_label>>-check}
# check code
grader::check_code()
```
" , .open = "<<", .close = ">>"))
}

insert_exercise_check_result <- function() {
  random_chunk_label <- generate_random_chunk_label()

  rstudioapi::insertText(glue::glue("
```{r <<random_chunk_label>>, exercise = TRUE}
# student code

```

```{r <<random_chunk_label>>-hint-1}
# hint text

```

```{r <<random_chunk_label>>-hint-2}
# hint text

```

```{r <<random_chunk_label>>-check}
grader::check_result(
  grader::pass_if(),
  grader::fail_if()
)
```
" , .open = "<<", .close = ">>"))
}

generate_random_chunk_label <- function() {
  paste0(sample(letters, 16, replace = TRUE), collapse = '')
}
