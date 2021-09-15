# gradethis (development version)

* New function: `grade_this(expr)`. Evaluates the expression and returns the first grade that is called or error that is thrown.
* New function: `grade_this_code(correct, incorrect)`. Makes a grade from comparing the user code against the solution code. This is a simplified version of `grade_code()`.
* New function: `code_feedback()`. Compares the user code against the solution code.
* Checking exercise code with blanks, e.g. `____`, now returns clear feedback that students should replace the `____` with code. (#153)
* The `exercise.parse.error` global option now accepts a function with one argument. The function is given the gradethis check environment with an additional `.error` object containing the parse error condition. (#153)
* Improved code feedback for function definitions will detect mistakes in function arguments (#178)
* gradethis now accepts markdown or an htmltools `tags` or a `tagList()` object for feedback messages. Markdown processing is handled via commonmark. Internally, all code feedback messages now use markdown. (#189)
* `pass_if_equal()` now compares the submitted `.result` to the exercise `.solution` by default (#203).
* New function: `debug_this()` returns information about the various objects available for use by grading code. It is designed for tutorial authors to use during development and can be used in `*-check` chunks or inside `grade_this()` grading code and the feedback is shown in the tutorial when clicking on **Submit Answer**. See `?debug_this` for more information. (#203)
* `graded()` and its pass/fail helper functions now accept `type` and `location` parameters that specify how the feedback is displayed (see [learnr Custom Checking](https://rstudio.github.io/learnr/exercises.html#Custom_checking) for more details about these options).
* Calling `gradethis_setup()` is no longer required if you want to use the default gradethis setup. Simply call `library(gradethis)`. You can use `gradethis_setup()` to adjust the default values of any options. (#210)
* New function: `give_code_feedback()`. When applied to a `grade_this()` or `grade_result()` grading function, code feedback is added to the messages of any `fail()` grades. (#219)
* All _failing_ `graded()` helper functions, like `fail()` etc, now take a `hint` argument that when `TRUE` adds a code feedback hint to the custom feedback message. The default value of this argument can be set via `gradethis_setup()`. (#216)
* Passing and failing `graded()` helper functions gain a `praise` or `encourage` argument (respectively) to prepend a random praising phrase when passing or append a random encouraging phrase when failing. The default values of these arguments can be set via `gradethis_setup()`. (#227)
* New functions: `give_praise()` and `give_encouragement()`. Follow the same pattern as `give_code_feedback()` to automatically add praise or encouragement to `pass()` or `fail()` grades, respectively. (#227)
* New function: `fail_if_code_feedback()`. Returns an _incorrect_ grade when there are differences between the user code and solution code (if it exists). (#228)
* We now use placeholder sentinel objects as function argument defaults to signal that a function will find the object in the `grade_this()` check environment. The help page `?grade_this-objects` describes these objects and documents their purpose, which you can also find by calling one of the placeholders, e.g. `.result`. (#232)
* Added `gradethis_error_checker()`, a more robust checking function for general use when the student's submission _should not_ throw an error. (#234)

### Breaking changes

* Deprecated `grade_feedback()`
* `graded()` now returns and signals a condition with class `"gradethis_graded"` instead of returning an object with class `"grader_graded"`
* `grade_code()`, `grade_result()`, and `grade_result_strict()` now return a function that accepts checking arguments to be supplied by `grade_learnr()`
* `grade_code()` will now throw an error (rather than returning `NULL`) if no solution code is provided
* `evaluate_condition()` now accepts `last_value` and `env` rather than `grader_args` and `learnr_args`
* `condition()`s now have a class of `"gradethis_condition"`
* gradethis now uses learnr for `random_praise()` and `random_encouragement()`. `random_encourage()` has been soft-deprecated (#183).
* The `gradethis.glue_pipe` option is now called `gradethis.pipe_warning` as it sets the default value of the `pipe_warning()` function. `pipe_warning()` can be included in the glue strings of other messages, such as those set by `gradethis.code.incorrect` (#193).
* The `glue_pipe` argument of `glue_code()` is now deprecated (#193).
* The "Insert Exercise ..." RStudio Addins were removed from gradethis (#196).
* The names of several global options were changed in #210. This will only affect users who were setting the options directly rather than using `gradethis_setup()`. The name changes are:
    - `gradethis.code.partial_matching` is now `gradethis.allow_partial_matching`
    - `gradethis.code.feedback` is now `gradethis.maybe_code_feedback`
    - `gradethis.code.correct` is now `gradethis.code_correct`
    - `gradethis.code.incorrect` is now `gradethis.code_incorrect`
    - `gradethis_glue_correct` is now `gradethis.glue_correct`
    - `gradethis_glue_incorrect` is now `gradethis.glue_incorrect`
    - `gradethis_glue_correct_test` is now `gradethis.glue_correct_test`
    - `gradethis_glue_incorrect_test` is now `gradethis.glue_incorrect_test`
* `grade_learnr()` is now called `gradethis_exercise_checker()`. `grade_learnr()` will continue to work but will issue a deprecation warning (#204).
* `gradethis_setup()` now uses a new argument order that favors the gradethis-specific options (#212).
* The `space_before` and `space_after` arguments of `maybe_code_feedback()` have been deprecated in favor of more flexible arguments `before` and `after` that accept arbitrary strings to add before or after the message. (#219)
* The `x` argument of `pass_if()` and `fail_if()` has been renamed `cond` and both functions now work inside `grade_this()`, although the function and formula versions are not supported there. (#216)
* `grade_this()` no longer automatically converts errors to `fail()` grades, instead authors need to wrap unit-test-style code in `fail_if_error()` to convert them to grades. This helps to better differentiate between unexpected errors resulting from the author's grading code and portions of the grading code where errors are expected and indicative of a problem with the user's code. (#254)

### Bug fixes
* Added a `NEWS.md` file to track changes to the package.
