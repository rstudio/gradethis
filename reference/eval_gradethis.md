# Capture grades and errors

Capture the first
[`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
signal or error thrown when evaluating the `expr`.

## Usage

``` r
eval_gradethis(expr, on_error = NULL, on_graded = NULL)
```

## Arguments

- expr:

  The expression or code block to evaluate

- on_error:

  A [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
  handler for class `error` with signature `function(error, this_env)`
  that receives the error object and calling environment of the error
  handler. `on_error` should use
  [`rlang::return_from()`](https://rlang.r-lib.org/reference/return_from.html)
  using `this_env` to immediately return the value and not continue
  evaluation.

- on_graded:

  A [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
  handler for class `graded` with signature `function(grade, this_env)`
  that receives the error object and calling environment of the error
  handler. `on_graded` should use
  [`rlang::return_from()`](https://rlang.r-lib.org/reference/return_from.html)
  using `this_env` to immediately return the value and not continue
  evaluation.

## Examples

``` r
# Passes with "message 1", short-circuiting evaluation
eval_gradethis({
  pass("message 1")
  pass("message 2")
  pass("message 3")
})
#> <gradethis_graded: [Correct] message 1>

# Fails with message from fail()
eval_gradethis({
  fail("incorrect")
  pass("correct")
})
#> <gradethis_graded: [Incorrect] incorrect>

# Fails with message from expect_true()
eval_gradethis({
  testthat::expect_true(FALSE)
  pass("message 2")
  pass("message 3")
})
#> Error in (function (e) {    on_error(e, this_env)})(structure(list(message = "Expected FALSE to be TRUE.\nDifferences:\n`actual`:   \033[32mFALSE\033[39m\n`expected`: \033[32mTRUE\033[39m \n",     srcref = NULL, trace = structure(list(call = list(pkgdown::build_site_github_pages(new_process = FALSE,         install = FALSE), build_site(pkg, preview = FALSE, install = install,         new_process = new_process, ...), build_site_local(pkg = pkg,         examples = examples, run_dont_run = run_dont_run, seed = seed,         lazy = lazy, override = override, preview = preview,         devel = devel, quiet = quiet), build_reference(pkg, lazy = lazy,         examples = examples, run_dont_run = run_dont_run, seed = seed,         override = override, preview = FALSE, devel = devel),         unwrap_purrr_error(purrr::map(topics, build_reference_topic,             pkg = pkg, lazy = lazy, examples_env = examples_env,             run_dont_run = run_dont_run)), withCallingHandlers(code,             purrr_error_indexed = function(err) {                cnd_signal(err$parent)            }), purrr::map(topics, build_reference_topic, pkg = pkg,             lazy = lazy, examples_env = examples_env, run_dont_run = run_dont_run),         map_("list", .x, .f, ..., .progress = .progress), with_indexed_errors(i = i,             names = names, error_call = .purrr_error_call, call_with_cleanup(map_impl,                 environment(), .type, .progress, n, names, i)),         withCallingHandlers(expr, error = function(cnd) {            if (i == 0L) {            }            else {                message <- c(i = "In index: {i}.")                if (!is.null(names) && !is.na(names[[i]]) &&                   names[[i]] != "") {                  name <- names[[i]]                  message <- c(message, i = "With name: {name}.")                }                else {                  name <- NULL                }                cli::cli_abort(message, location = i, name = name,                   parent = cnd, call = error_call, class = "purrr_error_indexed")            }        }), call_with_cleanup(map_impl, environment(), .type,             .progress, n, names, i), .f(.x[[i]], ...), withCallingHandlers(data_reference_topic(topic,             pkg, examples_env = examples_env, run_dont_run = run_dont_run),             error = function(err) {                cli::cli_abort("Failed to parse Rd in {.file {topic$file_in}}",                   parent = err, call = quote(build_reference()))            }), data_reference_topic(topic, pkg, examples_env = examples_env,             run_dont_run = run_dont_run), run_examples(tags$tag_examples[[1]],             env = if (is.null(examples_env)) NULL else new.env(parent = examples_env),             topic = tools::file_path_sans_ext(topic$file_in),             run_dont_run = run_dont_run), highlight_examples(code,             topic, env = env), downlit::evaluate_and_highlight(code,             fig_save = fig_save_topic, env = eval_env, output_handler = handler),         evaluate::evaluate(code, child_env(env), new_device = TRUE,             output_handler = output_handler), withRestarts(with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval_continue = function() TRUE, eval_stop = function() FALSE),         withRestartList(expr, restarts), withOneRestart(withRestartList(expr,             restarts[-nr]), restarts[[nr]]), doWithOneRestart(return(expr),             restart), withRestartList(expr, restarts[-nr]), withOneRestart(expr,             restarts[[1L]]), doWithOneRestart(return(expr), restart),         with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval(call), eval(call), withCallingHandlers(code,             message = `<fn>`, warning = `<fn>`, error = `<fn>`),         withVisible(eval(expr, envir)), eval(expr, envir), eval(expr,             envir), eval_gradethis({            testthat::expect_true(FALSE)            pass("message 2")            pass("message 3")        }), capture_graded(on_graded = on_graded, capture_errors(on_error = on_error,             expr)), withCallingHandlers(gradethis_graded = function(grade) {            on_graded(grade, this_env)        }, expr), capture_errors(on_error = on_error, expr),         withCallingHandlers(error = function(e) {            on_error(e, this_env)        }, expr), testthat::expect_true(FALSE)), parent = c(0L,     1L, 2L, 3L, 4L, 5L, 4L, 7L, 8L, 9L, 8L, 8L, 12L, 12L, 14L,     15L, 16L, 17L, 18L, 19L, 20L, 21L, 20L, 23L, 24L, 18L, 26L,     27L, 26L, 18L, 18L, 31L, 32L, 33L, 34L, 33L, 36L, 32L), visible = c(TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), namespace = c("pkgdown",     "pkgdown", "pkgdown", "pkgdown", "pkgdown", "base", "purrr",     "purrr", "purrr", "base", "purrr", "pkgdown", "base", "pkgdown",     "pkgdown", "pkgdown", "downlit", "evaluate", "base", "base",     "base", "base", "base", "base", "base", "evaluate", "base",     "base", "base", "base", "base", "base", "gradethis", "gradethis",     "base", "gradethis", "base", "testthat"), scope = c("::",     "::", ":::", "::", ":::", "::", "::", ":::", ":::", "::",     ":::", "local", "::", ":::", ":::", ":::", "::", "::", "::",     "local", "local", "local", "local", "local", "local", ":::",     "::", "::", "::", "::", "::", "::", "::", ":::", "::", ":::",     "::", "::")), row.names = c(NA, -38L), version = 2L, class = c("rlang_trace",     "rlib_trace", "tbl", "data.frame"))), class = c("expectation_failure", "expectation", "error", "condition"))): Expected FALSE to be TRUE.
#> Differences:
#> `actual`:   FALSE
#> `expected`: TRUE 
#> <gradethis_graded: [Neutral]
#>   A problem occurred with the grading code for this exercise.
#> >

# Fails immediately with message "boom"
eval_gradethis({
  stop("boom")
  pass("message 2")
  pass("message 3")
})
#> Error in stop("boom"): boom
#> <gradethis_graded: [Neutral]
#>   A problem occurred with the grading code for this exercise.
#> >
```
