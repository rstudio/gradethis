test_that("gradethis_setup() sets `grading_problem.type` and `grading_problem.message`", {
  with_options(
    list(
      gradethis.grading_problem.type = NULL,
      gradethis.grading_problem.message = NULL
    ), {
      gradethis_setup(
        grading_problem.type = "info",
        grading_problem.message = "TEST PASS"
      )
      expect_equal(gradethis_settings$grading_problem.type(), "info")
      expect_equal(gradethis_settings$grading_problem.message(), "TEST PASS")
    }
  )
})

test_that("gradethis_setup() issues warning for invalid `grading_problem.type`", {
  with_options(list(gradethis.grading_problem.type = NULL), {
    testthat::expect_message(
      gradethis_setup(grading_problem.type = "bad"),
      'Defaulting to "warning"'
    )
    expect_equal(
      gradethis_settings$grading_problem.type(),
      gradethis_default_options$grading_problem.type
    )
  })

  with_options(list(gradethis.grading_problem.type = "PASS"), {
    testthat::expect_message(
      gradethis_setup(grading_problem.type = "bad"),
      'Defaulting to "warning"'
    )
  })
})

test_that("gradethis_setup() sets default learnr options via chunk opts", {
  local_knitr_opts_chunk()

  with_knitr_opts_chunk(list(exercise.checker = "fail"), {
    gradethis_setup(exercise.checker = "PASS")
    expect_equal(knitr::opts_chunk$get("exercise.checker"), "PASS")
  })

  with_knitr_opts_chunk(list(exercise.checker = "fail"), {
    gradethis_setup(exercise.checker = "PASS", exercise.timelimit = 42)
    expect_equal(knitr::opts_chunk$get("exercise.checker"), "PASS")
    expect_equal(knitr::opts_chunk$get("exercise.timelimit"), 42)
  })
})

test_that("gradethis_setup() sets time limit for waldo::compare()", {
  local_knitr_opts_chunk()

  with_options(list(gradethis.diff.timelimit = NULL), {
    with_knitr_opts_chunk(list(exercise.timelimit = NULL), {
      gradethis_setup()
      expect_equal(
        gradethis_settings$diff.timelimit() %||%
          (knitr::opts_chunk$get("exercise.timelimit") * 0.8),
        48
      )
    })
  })

  with_options(list(gradethis.diff.timelimit = NULL), {
    with_knitr_opts_chunk(list(exercise.timelimit = NULL), {
      gradethis_setup(exercise.timelimit = 10)
      expect_equal(
        gradethis_settings$diff.timelimit() %||%
          (knitr::opts_chunk$get("exercise.timelimit") * 0.8),
        8
      )
    })
  })

  with_options(list(gradethis.diff.timelimit = NULL), {
    with_knitr_opts_chunk(list(exercise.timelimit = NULL), {
      gradethis_setup(diff.timelimit = 10)
      expect_equal(
        gradethis_settings$diff.timelimit() %||%
          (knitr::opts_chunk$get("exercise.timelimit") * 0.8),
        10
      )
    })
  })

  with_options(list(gradethis.diff.timelimit = NULL), {
    with_knitr_opts_chunk(list(exercise.timelimit = NULL), {
      gradethis_setup(exercise.timelimit = 10, diff.timelimit = 10)
      expect_equal(
        gradethis_settings$diff.timelimit() %||%
          (knitr::opts_chunk$get("exercise.timelimit") * 0.8),
        10
      )
    })
  })
})
