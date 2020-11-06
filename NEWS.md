# gradethis development

### Breaking changes

* Deprecated `grade_feedback()`
* `graded()` now returns and signals a condition with class `"gradethis_graded"` instead of returning an object with class `"grader_graded"`
* `grade_code()`, `grade_result()`, and `grade_result_strict()` now return a function that accepts checking arguments to be supplied by `grade_learnr()`
* `grade_code()` will now throw an error (rather than returning `NULL`) if no solution code is provided
* `evaluate_condition()` now accepts `last_value` and `env` rather than `grader_args` and `learnr_args`
* `condition()`s now have a class of `"gradethis_condition"`


### Bug fixes
* Added a `NEWS.md` file to track changes to the package.
