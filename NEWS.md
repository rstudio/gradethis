# gradethis development

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
    - `gradethis.code.feedback` is now `gradethis.fail_code_feedback`
    - `gradethis.code.correct` is now `gradethis.code_correct`
    - `gradethis.code.incorrect` is now `gradethis.code_incorrect`
    - `gradethis_glue_correct` is now `gradethis.glue_correct`
    - `gradethis_glue_incorrect` is now `gradethis.glue_incorrect`
    - `gradethis_glue_correct_test` is now `gradethis.glue_correct_test`
    - `gradethis_glue_incorrect_test` is now `gradethis.glue_incorrect_test`
* `grade_learnr()` is now called `gradethis_exercise_checker()`. `grade_learnr()` will continue to work but will issue a deprecation warning (#204).


### Bug fixes
* Added a `NEWS.md` file to track changes to the package.
