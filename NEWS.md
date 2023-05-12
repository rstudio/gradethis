# gradethis (development version)

# gradethis 0.2.12.9004

* Add `fail_if_not_equal()` (#346).
* `pass_if_equal()`, `fail_if_equal()`, and `fail_if_not_equal()` now call `gradethis_equal()`, an S3 generic which calls `waldo::compare()` by default. This allows other methods for `gradethis_equal()` to handle special cases (#346).

# gradethis 0.2.12.9003

* `call_standardise_formals()` now attempts to standardize arguments passed through `...` in mapping functions like `lapply()` or `purrr::map()` (#344).

# gradethis 0.2.12.9002

* `call_standardise_formals()` now attempts to standardize the arguments of calls to S3 generics (#339).

# gradethis 0.2.12.9001

* `pass_if()` and `fail_if()` now produce more informative error messages if their `cond` argument is invalid (#341).

# gradethis 0.2.12.9000

* New functions: `user_object_get()`, `user_object_exists()` and `user_object_list()` can be used to interact with objects created by the student's code. `solution_object_get()`, `solution_object_exists()` and `solution_object_list()` do the same for objects created by the solution code (#333).
* New function: `with_exercise()` allows you to evaluate an expression as if it were run inside `grade_this()` with an associated exercise. It can be used alongside `mock_this_exercise()` for testing grading code outside the context of a learnr tutorial (#333).
* `.envir_solution` is now included in the `grade_this()` check environment alongside `.envir_prep` and `.envir_result`. `.envir_solution` contains the state of the environment used to execute solution code, just as `.envir_result` does for student code (#333).

# gradethis 0.2.11.9000

## Bug fixes

* Code feedback is now disabled for non-R exercise engines (#321).
* More operators were added to our list of infixes (#327).

# gradethis 0.2.10.9000

* Feedback for error messages are now slightly more generic and refer to _your code_ instead of _your R code_ (@Laura-Puckett #318).

# gradethis 0.2.9.9000

* `code_feedback()` now supports multiple solutions. By default, `code_feedback()` now looks for `.solution_code_all`. If multiple solutions are present, string distance is used to determine the closest solution to `.user_code` and give feedback based on that solution. Functions that call `code_feedback()` internally (`grade_this_code()`, `fail_if_code_feedback()` and `maybe_code_feedback()`) inherit the same functionality (#289).
* The `solution_code` argument of `code_feedback()` is now the single entry-point for solution code in `code_feedback()`, `fail_if_code_feedback()` and other functions that work with both `.solution_code` and `.solution_code_all`. In these cases, the default argument values will use multiple solutions if they exist (#305).
* `pass_if_equal()` supports multiple solutions when authors set `y = .solution_all` to compare the student's result with all solutions. For additional details, please see the new section, _Comparing with Multiple Solutions_, in `?pass_if_equal` (#306).
* A new `vignette("multiple_solutions")` describes how gradethis can be used to provide feedback for exercises with more than one solution (#312).
* `gradethis_error_checker()` gains the `hint` argument from `fail()` which follows the global `gradethis.fail.hint` option. When `FALSE`, the error feedback won't include code feedback hints (thanks @cswclui, #315).
* `gradethis_exercise_checker()` can now be configure to evaluate solution code for non-R exercise engines by providing a function of `code` and `envir` to the `solution_eval_fn` argument. In addition to evaluation R exercise solutions, gradethis will now also evaluate SQL exercise solutions (#316).

# gradethis 0.2.8.9000

* gradethis now includes support for multiple solutions with different results. The solutions are made available in the `.solution_all` object in `grade_this()` grading code. `pass_if_equal()` gains support for multiple solutions as its `y` argument. If `.solution_all` is used as the `y` argument of `pass_if_equal()`, it will return a passing grade if `x` matches any of the multiple solutions (#296).

# gradethis 0.2.6.9000

* You may now call `return()` in `grade_this()` grading code to exit grading early. This is allowed in code and error checking code, but will result in an "internal error" when used in the `-check` chunk grading code (#284).
* gradethis now includes low-level support for multiple solutions. Authors can add multiple solutions in the `-solution` chunk, separated by [code section headers](https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections-in-the-RStudio-IDE), e.g. `# ----`. (note only trailing dashes, `----`, are supported). These additional solutions are made available in the `.solution_code_all` object in `grade_this()` grading code and are named with the code section name if one is provided, e.g. `# first ----`. When multiple solutions are provided using the code section comments, `.solution` and `.solution_code` will default to the **last** solution. (#286)
* `grade_this_code()` and `fail_if_code_feedback()` now return informative feedback with a neutral grade when no code is submitted and when not previously caught by learnr (#288).
* gradethis can now be used to grade non-R code. If learnr can evaluate the non-R code and return the result of the submitted code as an R object, then gradethis can be used to grade the submission result. Grading code is still written in R and code feedback tool designed for R will not work as expected (#290).
* `pass_if_equal()` and `fail_if_equal()` gain a tolerance argument which is passed to `waldo::compare()`. This defaults to the same default value as `all.equal()` to avoid floating point errors when comparing numeric values (#295).

# gradethis 0.2.5.9000

* `grade_this_code()` gains an `action` argument, allowing authors to choose if `grade_this_code()` should only `"pass"` or `"fail"` the user's submission. By default, `grade_this_code()` uses `action = "both"` to maintain current behavior. (#276)
* When combined with learnr version 0.10.1.9017 or later, gradethis' exercise checking function will not require that grading code absolutely return feedback unless exercise checking is at the `"check"` stage. (#276)

## Breaking changes

* Errors in the grading code are now returned as _neutral_ grades rather than failing grades. The feedback message and type can be changed with two new arguments to `gradethis_setup()`: `grading_problem.message` and `grading_problem.type` (#256).

# gradethis 0.2.4.9000

* Added `gradethis_error_checker()`, a more robust checking function for general use when the student's submission _should not_ throw an error. (#234)

## Breaking changes

* `grade_this()` no longer automatically converts errors to `fail()` grades, instead authors need to wrap unit-test-style code in `fail_if_error()` to convert them to grades. This helps to better differentiate between unexpected errors resulting from the author's grading code and portions of the grading code where errors are expected and indicative of a problem with the user's code. (#254)

# gradethis 0.2.3.9001

* All _failing_ `graded()` helper functions, like `fail()` etc, now take a `hint` argument that when `TRUE` adds a code feedback hint to the custom feedback message. The default value of this argument can be set via `gradethis_setup()`. (#216)
* Passing and failing `graded()` helper functions gain a `praise` or `encourage` argument (respectively) to prepend a random praising phrase when passing or append a random encouraging phrase when failing. The default values of these arguments can be set via `gradethis_setup()`. (#227)
* New functions: `give_praise()` and `give_encouragement()`. Follow the same pattern as `give_code_feedback()` to automatically add praise or encouragement to `pass()` or `fail()` grades, respectively. (#227)
* New function: `fail_if_code_feedback()`. Returns an _incorrect_ grade when there are differences between the user code and solution code (if it exists). (#228)
* We now use placeholder sentinel objects as function argument defaults to signal that a function will find the object in the `grade_this()` check environment. The help page `?grade_this-objects` describes these objects and documents their purpose, which you can also find by calling one of the placeholders, e.g. `.result`. (#232)

## Breaking changes

* The `x` argument of `pass_if()` and `fail_if()` has been renamed `cond` and both functions now work inside `grade_this()`, although the function and formula versions are not supported there. (#216)

# gradethis 0.2.3.9000

* New function: `give_code_feedback()`. When applied to a `grade_this()` or `grade_result()` grading function, code feedback is added to the messages of any `fail()` grades. (#219)

## Breaking changes

* `gradethis_setup()` now uses a new argument order that favors the gradethis-specific options (#212).
* `gradethis.code.feedback` is now `gradethis.maybe_code_feedback`. (#219)
* The `space_before` and `space_after` arguments of `maybe_code_feedback()` have been deprecated in favor of more flexible arguments `before` and `after` that accept arbitrary strings to add before or after the message. (#219)

# gradethis 0.2.2.9000

* Calling `gradethis_setup()` is no longer required if you want to use the default gradethis setup. Simply call `library(gradethis)`. You can use `gradethis_setup()` to adjust the default values of any options. (#210)

## Breaking changes

* The "Insert Exercise ..." RStudio Addins were removed from gradethis (#196).
* The names of several global options were changed in #210. This will only affect users who were setting the options directly rather than using `gradethis_setup()`. The name changes are:
    - `gradethis.code.partial_matching` is now `gradethis.allow_partial_matching`
    - `gradethis.code.feedback` is now `gradethis.fail_code_feedback`
    - `gradethis.code.correct` is now `gradethis.code_correct`
    - `gradethis.code.incorrect` is now `gradethis.code_incorrect`
    - `gradethis_glue_correct` is now `gradethis.glue_correct`
    - `gradethis_glue_incorrect` is now `gradethis.glue_incorrect`
    - `gradethis_glue_correct_test` is now `gradethis.glue_correct_test`
    - `gradethis_glue_incorrect_test` is now `gradethis.glue_incorrect_test`
* `grade_learnr()` is now called `gradethis_exercise_checker()`. `grade_learnr()` will continue to work but will issue a deprecation warning (#204).

# gradethis 0.2.1.9000

* `pass_if_equal()` now compares the submitted `.result` to the exercise `.solution` by default (#203).
* New function: `debug_this()` returns information about the various objects available for use by grading code. It is designed for tutorial authors to use during development and can be used in `*-check` chunks or inside `grade_this()` grading code and the feedback is shown in the tutorial when clicking on **Submit Answer**. See `?debug_this` for more information. (#203)
* `graded()` and its pass/fail helper functions now accept `type` and `location` parameters that specify how the feedback is displayed (see [learnr Custom Checking](https://rstudio.github.io/learnr/exercises.html#Custom_checking) for more details about these options).

## Bug fixes

* The `gradethis.glue_pipe` option is now called `gradethis.pipe_warning` as it sets the default value of the `pipe_warning()` function. `pipe_warning()` can be included in the glue strings of other messages, such as those set by `gradethis.code.incorrect` (#193).
* The `glue_pipe` argument of `glue_code()` is now deprecated (#193).

# gradethis 0.2.0.9001

* Checking exercise code with blanks, e.g. `____`, now returns clear feedback that students should replace the `____` with code. (#153)
* The `exercise.parse.error` global option now accepts a function with one argument. The function is given the gradethis check environment with an additional `.error` object containing the parse error condition. (#153)
* Improved code feedback for function definitions will detect mistakes in function arguments (#178)
* gradethis now accepts markdown or an htmltools `tags` or a `tagList()` object for feedback messages. Markdown processing is handled via commonmark. Internally, all code feedback messages now use markdown. (#189)

## Breaking changes

* Improved code feedback for function definitions will detect mistakes in function arguments (#178)
* gradethis now uses learnr for `random_praise()` and `random_encouragement()`. `random_encourage()` has been soft-deprecated (#183).

# gradethis 0.2.0.9000

* New function: `grade_this(expr)`. Evaluates the expression and returns the first grade that is called or error that is thrown.
* New function: `grade_this_code(correct, incorrect)`. Makes a grade from comparing the user code against the solution code. This is a simplified version of `grade_code()`.
* New function: `code_feedback()`. Compares the user code against the solution code.

## Breaking changes

* Deprecated `grade_feedback()`
* `graded()` now returns and signals a condition with class `"gradethis_graded"` instead of returning an object with class `"grader_graded"`
* `grade_code()`, `grade_result()`, and `grade_result_strict()` now return a function that accepts checking arguments to be supplied by `grade_learnr()`
* `grade_code()` will now throw an error (rather than returning `NULL`) if no solution code is provided
* `evaluate_condition()` now accepts `last_value` and `env` rather than `grader_args` and `learnr_args`
* `condition()`s now have a class of `"gradethis_condition"`

## Bug fixes

* Added a `NEWS.md` file to track changes to the package.
