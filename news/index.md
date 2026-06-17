# Changelog

## gradethis 0.2.14

- [`grade_code()`](https://rstudio.github.io/gradethis/reference/grade_code.md)
  no longer fails if `.envir_result` or `.envir_solution` is missing
  ([\#355](https://github.com/rstudio/gradethis/issues/355)).
- `detect_mistakes()` now keeps a version of standardized user and
  solution code with and without default arguments added. Missing
  arguments are detected by comparing the user code with defaults to the
  solution code without defaults. Surplus arguments are detected by
  comparing the user code without defaults to the solution code with
  defaults ([\#356](https://github.com/rstudio/gradethis/issues/356)).
  - This helps avoid spurious feedback when comparing code that involves
    S3 methods. If the user’s code differs from the solution code in a
    way that means a different S3 method is used, the standardized code
    may gain different default arguments. This could result in feedback
    about missing or surplus arguments that were added by code
    standardization rather than by the student, which is not actionable
    feedback. By no longer looking for default arguments that are
    missing or surplus in the user code, we ensure that students receive
    more actionable feedback, likely about the incorrect argument that
    resulted in the use of a different S3 method.
- The
  [`gradethis_equal.list()`](https://rstudio.github.io/gradethis/reference/gradethis_equal.md)
  method is now only used if both `x` and `y` are bare lists (as defined
  by
  [`rlang::is_bare_list()`](https://rlang.r-lib.org/reference/bare-type-predicates.html))
  ([\#357](https://github.com/rstudio/gradethis/issues/357)).
  - This fixes a bug where a list could be marked as equal to another
    object with the same contents but a different class,
    e.g. `list(a = 1, b = 2)` and `c(a = 1, b = 2)` or
    `data.frame(a = 1, b = 2)`.
- Fix bug where `call_standardise_formals()` would fail when given a
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
  function where `.f` is an index rather than a function
  ([\#359](https://github.com/rstudio/gradethis/issues/359)).

## gradethis 0.2.13

- [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  now standardizes arguments to functions defined within student and
  solution code before comparing code. It also now successfully
  standardizes arguments passed through `...` by mapping functions into
  functions defined by setup code
  ([\#349](https://github.com/rstudio/gradethis/issues/349)).
- [`gradethis_equal()`](https://rstudio.github.io/gradethis/reference/gradethis_equal.md)
  now has a method for objects of class `list`. If two lists are not
  [`identical()`](https://rdrr.io/r/base/identical.html) and their
  lengths are the same,
  [`gradethis_equal()`](https://rstudio.github.io/gradethis/reference/gradethis_equal.md)
  is applied to each element pairwise. This allows special logic for
  specific classes to be used for list elements of that class
  ([\#351](https://github.com/rstudio/gradethis/issues/351)).
- `call_standardise_formals()` now applies
  [`ggplot2::standardise_aes_names()`](https://ggplot2.tidyverse.org/reference/standardise_aes_names.html)
  to all arguments of `ggplot2` functions. This means it no longer
  detects inconsequential differences between, e.g., `color =` and
  `colour =` ([\#353](https://github.com/rstudio/gradethis/issues/353)).
- When an exercise without an `-error-check` chunk returns an error, the
  default feedback now includes both the error message and code feedback
  if `fail.hint = TRUE`. Previously the error message was only shown
  when `fail.hint = FALSE` and code feedback was only shown when
  `fail.hint = TRUE`
  ([\#352](https://github.com/rstudio/gradethis/issues/352)).

## gradethis 0.2.12.9005

- [`gradethis_equal()`](https://rstudio.github.io/gradethis/reference/gradethis_equal.md)
  now has default arguments of `x = .result` and `y = .solution`
  ([\#347](https://github.com/rstudio/gradethis/issues/347)).
  - [`gradethis_equal.default()`](https://rstudio.github.io/gradethis/reference/gradethis_equal.md)
    now has a default argument of
    `tolerance = sqrt(.Machine$double.eps)`.

## gradethis 0.2.12.9004

- Add
  [`fail_if_not_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  ([\#346](https://github.com/rstudio/gradethis/issues/346)).
- [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
  [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
  and
  [`fail_if_not_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  now call
  [`gradethis_equal()`](https://rstudio.github.io/gradethis/reference/gradethis_equal.md),
  an S3 generic which calls
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html)
  by default. This allows other methods for
  [`gradethis_equal()`](https://rstudio.github.io/gradethis/reference/gradethis_equal.md)
  to handle special cases
  ([\#346](https://github.com/rstudio/gradethis/issues/346)).

## gradethis 0.2.12.9003

- `call_standardise_formals()` now attempts to standardize arguments
  passed through `...` in mapping functions like
  [`lapply()`](https://rdrr.io/r/base/lapply.html) or
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
  ([\#344](https://github.com/rstudio/gradethis/issues/344)).

## gradethis 0.2.12.9002

- `call_standardise_formals()` now attempts to standardize the arguments
  of calls to S3 generics
  ([\#339](https://github.com/rstudio/gradethis/issues/339)).

## gradethis 0.2.12.9001

- [`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  and
  [`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  now produce more informative error messages if their `cond` argument
  is invalid ([\#341](https://github.com/rstudio/gradethis/issues/341)).

## gradethis 0.2.12.9000

- New functions:
  [`user_object_get()`](https://rstudio.github.io/gradethis/reference/user_object.md),
  [`user_object_exists()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  and
  [`user_object_list()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  can be used to interact with objects created by the student’s code.
  [`solution_object_get()`](https://rstudio.github.io/gradethis/reference/user_object.md),
  [`solution_object_exists()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  and
  [`solution_object_list()`](https://rstudio.github.io/gradethis/reference/user_object.md)
  do the same for objects created by the solution code
  ([\#333](https://github.com/rstudio/gradethis/issues/333)).
- New function:
  [`with_exercise()`](https://rstudio.github.io/gradethis/reference/with_exercise.md)
  allows you to evaluate an expression as if it were run inside
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  with an associated exercise. It can be used alongside
  [`mock_this_exercise()`](https://rstudio.github.io/gradethis/reference/mock_this_exercise.md)
  for testing grading code outside the context of a learnr tutorial
  ([\#333](https://github.com/rstudio/gradethis/issues/333)).
- `.envir_solution` is now included in the
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  check environment alongside `.envir_prep` and `.envir_result`.
  `.envir_solution` contains the state of the environment used to
  execute solution code, just as `.envir_result` does for student code
  ([\#333](https://github.com/rstudio/gradethis/issues/333)).

## gradethis 0.2.11.9000

### Bug fixes

- Code feedback is now disabled for non-R exercise engines
  ([\#321](https://github.com/rstudio/gradethis/issues/321)).
- More operators were added to our list of infixes
  ([\#327](https://github.com/rstudio/gradethis/issues/327)).

## gradethis 0.2.10.9000

- Feedback for error messages are now slightly more generic and refer to
  *your code* instead of *your R code*
  ([@Laura-Puckett](https://github.com/Laura-Puckett)
  [\#318](https://github.com/rstudio/gradethis/issues/318)).

## gradethis 0.2.9.9000

- [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  now supports multiple solutions. By default,
  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  now looks for `.solution_code_all`. If multiple solutions are present,
  string distance is used to determine the closest solution to
  `.user_code` and give feedback based on that solution. Functions that
  call
  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  internally
  ([`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md),
  [`fail_if_code_feedback()`](https://rstudio.github.io/gradethis/reference/fail_if_code_feedback.md)
  and
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md))
  inherit the same functionality
  ([\#289](https://github.com/rstudio/gradethis/issues/289)).
- The `solution_code` argument of
  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  is now the single entry-point for solution code in
  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md),
  [`fail_if_code_feedback()`](https://rstudio.github.io/gradethis/reference/fail_if_code_feedback.md)
  and other functions that work with both `.solution_code` and
  `.solution_code_all`. In these cases, the default argument values will
  use multiple solutions if they exist
  ([\#305](https://github.com/rstudio/gradethis/issues/305)).
- [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  supports multiple solutions when authors set `y = .solution_all` to
  compare the student’s result with all solutions. For additional
  details, please see the new section, *Comparing with Multiple
  Solutions*, in
  [`?pass_if_equal`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  ([\#306](https://github.com/rstudio/gradethis/issues/306)).
- A new
  [`vignette("multiple_solutions")`](https://rstudio.github.io/gradethis/articles/multiple_solutions.md)
  describes how gradethis can be used to provide feedback for exercises
  with more than one solution
  ([\#312](https://github.com/rstudio/gradethis/issues/312)).
- [`gradethis_error_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_error_checker.md)
  gains the `hint` argument from
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  which follows the global `gradethis.fail.hint` option. When `FALSE`,
  the error feedback won’t include code feedback hints (thanks
  [@cswclui](https://github.com/cswclui),
  [\#315](https://github.com/rstudio/gradethis/issues/315)).
- [`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md)
  can now be configure to evaluate solution code for non-R exercise
  engines by providing a function of `code` and `envir` to the
  `solution_eval_fn` argument. In addition to evaluation R exercise
  solutions, gradethis will now also evaluate SQL exercise solutions
  ([\#316](https://github.com/rstudio/gradethis/issues/316)).

## gradethis 0.2.8.9000

- gradethis now includes support for multiple solutions with different
  results. The solutions are made available in the `.solution_all`
  object in
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  grading code.
  [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  gains support for multiple solutions as its `y` argument. If
  `.solution_all` is used as the `y` argument of
  [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md),
  it will return a passing grade if `x` matches any of the multiple
  solutions ([\#296](https://github.com/rstudio/gradethis/issues/296)).

## gradethis 0.2.6.9000

- You may now call [`return()`](https://rdrr.io/r/base/function.html) in
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  grading code to exit grading early. This is allowed in code and error
  checking code, but will result in an “internal error” when used in the
  `-check` chunk grading code
  ([\#284](https://github.com/rstudio/gradethis/issues/284)).
- gradethis now includes low-level support for multiple solutions.
  Authors can add multiple solutions in the `-solution` chunk, separated
  by [code section
  headers](https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections-in-the-RStudio-IDE),
  e.g. `# ----`. (note only trailing dashes, `----`, are supported).
  These additional solutions are made available in the
  `.solution_code_all` object in
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  grading code and are named with the code section name if one is
  provided, e.g. `# first ----`. When multiple solutions are provided
  using the code section comments, `.solution` and `.solution_code` will
  default to the **last** solution.
  ([\#286](https://github.com/rstudio/gradethis/issues/286))
- [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
  and
  [`fail_if_code_feedback()`](https://rstudio.github.io/gradethis/reference/fail_if_code_feedback.md)
  now return informative feedback with a neutral grade when no code is
  submitted and when not previously caught by learnr
  ([\#288](https://github.com/rstudio/gradethis/issues/288)).
- gradethis can now be used to grade non-R code. If learnr can evaluate
  the non-R code and return the result of the submitted code as an R
  object, then gradethis can be used to grade the submission result.
  Grading code is still written in R and code feedback tool designed for
  R will not work as expected
  ([\#290](https://github.com/rstudio/gradethis/issues/290)).
- [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  and
  [`fail_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  gain a tolerance argument which is passed to
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html).
  This defaults to the same default value as
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) to avoid
  floating point errors when comparing numeric values
  ([\#295](https://github.com/rstudio/gradethis/issues/295)).

## gradethis 0.2.5.9000

- [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
  gains an `action` argument, allowing authors to choose if
  [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
  should only `"pass"` or `"fail"` the user’s submission. By default,
  [`grade_this_code()`](https://rstudio.github.io/gradethis/reference/grade_this_code.md)
  uses `action = "both"` to maintain current behavior.
  ([\#276](https://github.com/rstudio/gradethis/issues/276))
- When combined with learnr version 0.10.1.9017 or later, gradethis’
  exercise checking function will not require that grading code
  absolutely return feedback unless exercise checking is at the
  `"check"` stage.
  ([\#276](https://github.com/rstudio/gradethis/issues/276))

### Breaking changes

- Errors in the grading code are now returned as *neutral* grades rather
  than failing grades. The feedback message and type can be changed with
  two new arguments to
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md):
  `grading_problem.message` and `grading_problem.type`
  ([\#256](https://github.com/rstudio/gradethis/issues/256)).

## gradethis 0.2.4.9000

- Added
  [`gradethis_error_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_error_checker.md),
  a more robust checking function for general use when the student’s
  submission *should not* throw an error.
  ([\#234](https://github.com/rstudio/gradethis/issues/234))

### Breaking changes

- [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  no longer automatically converts errors to
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  grades, instead authors need to wrap unit-test-style code in
  [`fail_if_error()`](https://rstudio.github.io/gradethis/reference/fail_if_error.md)
  to convert them to grades. This helps to better differentiate between
  unexpected errors resulting from the author’s grading code and
  portions of the grading code where errors are expected and indicative
  of a problem with the user’s code.
  ([\#254](https://github.com/rstudio/gradethis/issues/254))

## gradethis 0.2.3.9001

- All *failing*
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  helper functions, like
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  etc, now take a `hint` argument that when `TRUE` adds a code feedback
  hint to the custom feedback message. The default value of this
  argument can be set via
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).
  ([\#216](https://github.com/rstudio/gradethis/issues/216))
- Passing and failing
  [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  helper functions gain a `praise` or `encourage` argument
  (respectively) to prepend a random praising phrase when passing or
  append a random encouraging phrase when failing. The default values of
  these arguments can be set via
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).
  ([\#227](https://github.com/rstudio/gradethis/issues/227))
- New functions:
  [`give_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)
  and
  [`give_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md).
  Follow the same pattern as
  [`give_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  to automatically add praise or encouragement to
  [`pass()`](https://rstudio.github.io/gradethis/reference/graded.md) or
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  grades, respectively.
  ([\#227](https://github.com/rstudio/gradethis/issues/227))
- New function:
  [`fail_if_code_feedback()`](https://rstudio.github.io/gradethis/reference/fail_if_code_feedback.md).
  Returns an *incorrect* grade when there are differences between the
  user code and solution code (if it exists).
  ([\#228](https://github.com/rstudio/gradethis/issues/228))
- We now use placeholder sentinel objects as function argument defaults
  to signal that a function will find the object in the
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  check environment. The help page `?grade_this-objects` describes these
  objects and documents their purpose, which you can also find by
  calling one of the placeholders, e.g. `.result`.
  ([\#232](https://github.com/rstudio/gradethis/issues/232))

### Breaking changes

- The `x` argument of
  [`pass_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  and
  [`fail_if()`](https://rstudio.github.io/gradethis/reference/pass_if.md)
  has been renamed `cond` and both functions now work inside
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md),
  although the function and formula versions are not supported there.
  ([\#216](https://github.com/rstudio/gradethis/issues/216))

## gradethis 0.2.3.9000

- New function:
  [`give_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md).
  When applied to a
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  or
  [`grade_result()`](https://rstudio.github.io/gradethis/reference/grade_result.md)
  grading function, code feedback is added to the messages of any
  [`fail()`](https://rstudio.github.io/gradethis/reference/graded.md)
  grades. ([\#219](https://github.com/rstudio/gradethis/issues/219))

### Breaking changes

- [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  now uses a new argument order that favors the gradethis-specific
  options ([\#212](https://github.com/rstudio/gradethis/issues/212)).
- `gradethis.code.feedback` is now `gradethis.maybe_code_feedback`.
  ([\#219](https://github.com/rstudio/gradethis/issues/219))
- The `space_before` and `space_after` arguments of
  [`maybe_code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md)
  have been deprecated in favor of more flexible arguments `before` and
  `after` that accept arbitrary strings to add before or after the
  message. ([\#219](https://github.com/rstudio/gradethis/issues/219))

## gradethis 0.2.2.9000

- Calling
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  is no longer required if you want to use the default gradethis setup.
  Simply call
  [`library(gradethis)`](https://pkgs.rstudio.com/gradethis/). You can
  use
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md)
  to adjust the default values of any options.
  ([\#210](https://github.com/rstudio/gradethis/issues/210))

### Breaking changes

- The “Insert Exercise …” RStudio Addins were removed from gradethis
  ([\#196](https://github.com/rstudio/gradethis/issues/196)).
- The names of several global options were changed in
  [\#210](https://github.com/rstudio/gradethis/issues/210). This will
  only affect users who were setting the options directly rather than
  using
  [`gradethis_setup()`](https://rstudio.github.io/gradethis/reference/gradethis_setup.md).
  The name changes are:
  - `gradethis.code.partial_matching` is now
    `gradethis.allow_partial_matching`
  - `gradethis.code.feedback` is now `gradethis.fail_code_feedback`
  - `gradethis.code.correct` is now `gradethis.code_correct`
  - `gradethis.code.incorrect` is now `gradethis.code_incorrect`
  - `gradethis_glue_correct` is now `gradethis.glue_correct`
  - `gradethis_glue_incorrect` is now `gradethis.glue_incorrect`
  - `gradethis_glue_correct_test` is now `gradethis.glue_correct_test`
  - `gradethis_glue_incorrect_test` is now
    `gradethis.glue_incorrect_test`
- [`grade_learnr()`](https://rstudio.github.io/gradethis/reference/gradethis-deprecated.md)
  is now called
  [`gradethis_exercise_checker()`](https://rstudio.github.io/gradethis/reference/gradethis_exercise_checker.md).
  [`grade_learnr()`](https://rstudio.github.io/gradethis/reference/gradethis-deprecated.md)
  will continue to work but will issue a deprecation warning
  ([\#204](https://github.com/rstudio/gradethis/issues/204)).

## gradethis 0.2.1.9000

- [`pass_if_equal()`](https://rstudio.github.io/gradethis/reference/pass_if_equal.md)
  now compares the submitted `.result` to the exercise `.solution` by
  default ([\#203](https://github.com/rstudio/gradethis/issues/203)).
- New function:
  [`debug_this()`](https://rstudio.github.io/gradethis/reference/debug_this.md)
  returns information about the various objects available for use by
  grading code. It is designed for tutorial authors to use during
  development and can be used in `*-check` chunks or inside
  [`grade_this()`](https://rstudio.github.io/gradethis/reference/grade_this.md)
  grading code and the feedback is shown in the tutorial when clicking
  on **Submit Answer**. See
  [`?debug_this`](https://rstudio.github.io/gradethis/reference/debug_this.md)
  for more information.
  ([\#203](https://github.com/rstudio/gradethis/issues/203))
- [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  and its pass/fail helper functions now accept `type` and `location`
  parameters that specify how the feedback is displayed (see [learnr
  Custom
  Checking](https://rstudio.github.io/learnr/exercises.html#Custom_checking)
  for more details about these options).

### Bug fixes

- The `gradethis.glue_pipe` option is now called
  `gradethis.pipe_warning` as it sets the default value of the
  [`pipe_warning()`](https://rstudio.github.io/gradethis/reference/pipe_warning.md)
  function.
  [`pipe_warning()`](https://rstudio.github.io/gradethis/reference/pipe_warning.md)
  can be included in the glue strings of other messages, such as those
  set by `gradethis.code.incorrect`
  ([\#193](https://github.com/rstudio/gradethis/issues/193)).
- The `glue_pipe` argument of `glue_code()` is now deprecated
  ([\#193](https://github.com/rstudio/gradethis/issues/193)).

## gradethis 0.2.0.9001

- Checking exercise code with blanks, e.g. `____`, now returns clear
  feedback that students should replace the `____` with code.
  ([\#153](https://github.com/rstudio/gradethis/issues/153))
- The `exercise.parse.error` global option now accepts a function with
  one argument. The function is given the gradethis check environment
  with an additional `.error` object containing the parse error
  condition. ([\#153](https://github.com/rstudio/gradethis/issues/153))
- Improved code feedback for function definitions will detect mistakes
  in function arguments
  ([\#178](https://github.com/rstudio/gradethis/issues/178))
- gradethis now accepts markdown or an htmltools `tags` or a `tagList()`
  object for feedback messages. Markdown processing is handled via
  commonmark. Internally, all code feedback messages now use markdown.
  ([\#189](https://github.com/rstudio/gradethis/issues/189))

### Breaking changes

- Improved code feedback for function definitions will detect mistakes
  in function arguments
  ([\#178](https://github.com/rstudio/gradethis/issues/178))
- gradethis now uses learnr for
  [`random_praise()`](https://rstudio.github.io/gradethis/reference/praise.md)
  and
  [`random_encouragement()`](https://rstudio.github.io/gradethis/reference/praise.md).
  [`random_encourage()`](https://rstudio.github.io/gradethis/reference/gradethis-deprecated.md)
  has been soft-deprecated
  ([\#183](https://github.com/rstudio/gradethis/issues/183)).

## gradethis 0.2.0.9000

- New function: `grade_this(expr)`. Evaluates the expression and returns
  the first grade that is called or error that is thrown.
- New function: `grade_this_code(correct, incorrect)`. Makes a grade
  from comparing the user code against the solution code. This is a
  simplified version of
  [`grade_code()`](https://rstudio.github.io/gradethis/reference/grade_code.md).
- New function:
  [`code_feedback()`](https://rstudio.github.io/gradethis/reference/code_feedback.md).
  Compares the user code against the solution code.

### Breaking changes

- Deprecated
  [`grade_feedback()`](https://rstudio.github.io/gradethis/reference/gradethis-deprecated.md)
- [`graded()`](https://rstudio.github.io/gradethis/reference/graded.md)
  now returns and signals a condition with class `"gradethis_graded"`
  instead of returning an object with class `"grader_graded"`
- [`grade_code()`](https://rstudio.github.io/gradethis/reference/grade_code.md),
  [`grade_result()`](https://rstudio.github.io/gradethis/reference/grade_result.md),
  and
  [`grade_result_strict()`](https://rstudio.github.io/gradethis/reference/grade_result.md)
  now return a function that accepts checking arguments to be supplied
  by
  [`grade_learnr()`](https://rstudio.github.io/gradethis/reference/gradethis-deprecated.md)
- [`grade_code()`](https://rstudio.github.io/gradethis/reference/grade_code.md)
  will now throw an error (rather than returning `NULL`) if no solution
  code is provided
- [`evaluate_condition()`](https://rstudio.github.io/gradethis/reference/evaluate_condition.md)
  now accepts `last_value` and `env` rather than `grader_args` and
  `learnr_args`
- [`condition()`](https://rstudio.github.io/gradethis/reference/grade_result.md)s
  now have a class of `"gradethis_condition"`

### Bug fixes

- Added a `NEWS.md` file to track changes to the package.
