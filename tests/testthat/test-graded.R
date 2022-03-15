test_that("pass_if_equal() finds .result and .solution automatically", {
  env <- new.env()

  # missing .result
  pass1 <- testthat::expect_message(
    pass_if_equal(env = env), ".result", fixed = TRUE
  )
  expect_graded(
    pass1,
    is_correct = logical(0),
    msg = "problem occurred"
  )

  # missing .solution
  eval(quote(.result <- 12), envir = env)
  pass2 <- testthat::expect_message(
    pass_if_equal(env = env), ".solution", fixed = TRUE
  )
  expect_graded(
    pass2,
    is_correct = logical(0),
    msg = "problem occurred"
  )

  # Missing .solution but comparison value provided
  expect_silent(pass_if_equal(0, env = env))
  expect_condition(pass_if_equal(12, env = env))
  expect_match(pass_if_equal(12, env = env, message = "YES")$message, "YES", fixed = TRUE)

  # Has solution (not equal, doesn't pass)
  eval(quote(.solution <- 0), envir = env)
  expect_silent(pass_if_equal(env = env))

  # Has solution (equal, does pass)
  eval(quote(.solution <- 12), envir = env)
  expect_condition(pass_if_equal(env = env))
  expect_match(pass_if_equal(env = env, message = "YES")$message, "YES", fixed = TRUE)
})

test_that("fail_if_equal() finds .result", {
  env <- new.env()

  # missing .result
  fail1 <- testthat::expect_message(
    fail_if_equal(env = env), ".result", fixed = TRUE
  )
  expect_graded(
    fail1,
    is_correct = logical(),
    msg = "problem occurred"
  )

  # Has .result (not equal, doesn't fail)
  eval(quote(.result <- 12), envir = env)
  expect_silent(fail_if_equal(0, env = env))
  # Has .result (equal, does fail)
  expect_condition(fail_if_equal(12, env = env))
  expect_match(fail_if_equal(12, env = env, message = "YES")$message, "YES", fixed = TRUE)
})

test_that("pass_if_equal() in grade_this()", {
  grader <- grade_this({
    pass_if_equal(message = "YES")
    fail("NO")
  })

  # correct
  expect_graded(
    grader(mock_this_exercise(42, 42)),
    is_correct = TRUE,
    msg = "YES"
  )

  # incorrect
  expect_graded(
    grader(mock_this_exercise(42, 40)),
    is_correct = FALSE,
    msg = "NO"
  )

  # bad: no solution
  expect_graded(
    grader(mock_this_exercise(42)),
    is_correct = logical(),
    msg = "No solution is provided"
  )


  # bad: no .result object
  testthat::expect_message(
    expect_graded(
      grader(list(.user_code = "12")),
      is_correct = logical(),
      msg = "problem occurred"
    ),
    regexp = ".result",
    fixed = TRUE
  )

  # bad: no .solution object
  testthat::expect_message(
    expect_graded(
      grader(list(.result = "12")),
      is_correct = logical(),
      msg = "problem occurred"
    ),
    regexp = ".solution",
    fixed = TRUE
  )
})

test_that("fail_if_equal() in grade_this()", {
  grader <- grade_this({
    fail_if_equal(40, message = "NO")
    pass("YES")
  })

  expect_graded(
    grader(mock_this_exercise(42, 42)),
    msg = "YES",
    is_correct = TRUE
  )

  expect_graded(
    grader(mock_this_exercise(40, 42)),
    msg = "NO",
    is_correct = FALSE
  )

  # bad: no .result object
  testthat::expect_message(
    expect_graded(
      grader(list(.user_code = "12")),
      is_correct = logical(),
      msg = "problem occurred"
    ),
    regexp = ".result",
    fixed = TRUE
  )
})

test_that("fail_if_code_feedback() returns grade if code feedback", {
  # code feedback message by default
  expect_graded(
    fail_if_code_feedback(user_code = "x + y", solution_code = "x + z"),
    is_correct = FALSE,
    msg = code_feedback("x + y", "x + z")
  )

  # fails with message and feedback
  expect_graded(
    fail_if_code_feedback("x + y", "x + z", message = "zzz"),
    is_correct = FALSE,
    msg = paste("zzz", code_feedback("x + y", "x + z"))
  )

  # fails with message and no feedback
  expect_graded(
    fail_if_code_feedback("x + y", "x + z", message = "zzz", hint = FALSE),
    is_correct = FALSE,
    msg = "zzz"
  )

  # finds .user_code and .solution_code and also glues correctly
  expect_grade_this(
    {
      x <- "zzz"
      fail_if_code_feedback(message = "{x}")
      pass("TEST FAILED")
    },
    user_code = "x + y",
    solution_code = "x + z",
    setup_exercise = "x = 1; y = 2; z = 3",
    is_correct = FALSE,
    msg = paste("zzz", code_feedback("x + y", "x + z"))
  )

  # accesses grade_this env variables in glue message
  expect_grade_this(
    {
      fail_if_code_feedback(message = "{.result}.")
      pass("TEST FAILED")
    },
    user_code = "x + y",
    solution_code = "x + z",
    setup_exercise = "x = 1; y = 2; z = 3",
    is_correct = FALSE,
    msg = paste("3.", code_feedback("x + y", "x + z"))
  )

  # no feedback if hint = FALSE
  expect_false(
    grepl(
      "I expected",
      expect_grade_this(
        {
          fail_if_code_feedback(message = "TEST PASSED", hint = FALSE)
          pass("TEST FAILED")
        },
        user_code = "x + y",
        solution_code = "x + z",
        setup_exercise = "x = 1; y = 2; z = 3",
        is_correct = FALSE,
        msg = "TEST PASSED"
      )$message
    )
  )

  # turn off local feedback, added back in with give_code_feedback()
  expect_grade_this(
    {
      fail_if_code_feedback("x + y", "x + z", message = "zzz", hint = FALSE) %>%
        give_code_feedback()
      pass("TEST FAILED")
    },
    user_code = "x + y",
    solution_code = "x + z",
    setup_exercise = "x = 1; y = 2; z = 3",
    is_correct = FALSE,
    msg = paste("zzz", code_feedback("x + y", "x + z"))
  )

  # no-op if no feedback
  expect_grade_this(
    {
      fail_if_code_feedback(message = "zzz")
      pass("TEST PASSED")
    },
    user_code = "x + z",
    solution_code = "x + z",
    setup_exercise = "x = 1; y = 2; z = 3",
    is_correct = TRUE,
    msg = "TEST PASSED"
  )

  # no-op if no solution
  expect_grade_this(
    {
      fail_if_code_feedback(message = "zzz")
      pass("TEST PASSED")
    },
    user_code = "x + z",
    setup_exercise = "x = 1; y = 2; z = 3",
    is_correct = TRUE,
    msg = "TEST PASSED"
  )

  # info grade if no user code
  expect_grade_this(
    {
      fail_if_code_feedback(message = "zzz")
      pass("TEST PASSED")
    },
    user_code = "",
    setup_exercise = "x = 1; y = 2; z = 3",
    is_correct = logical(),
    msg = "I didn't receive your code"
  )

  # Expect teacher grading problem if called outside of grade_this()
  testthat::expect_message(
    expect_graded(
      fail_if_code_feedback(),
      is_correct = logical(),
      msg = "problem occurred"
    )
  )


  testthat::expect_message(
    expect_null(fail_if_code_feedback(user_code = "2")),
    "expected `.solution_code_all` to be found",
    fixed = TRUE
  )
})

test_that("fail_if_code_feedback() gives neutral feedback if code is missing", {
  expect_graded(
    grade_this(fail_if_code_feedback())(
      mock_this_exercise(.user_code = "", .solution_code = "rnorm(1)")
    ),
    is_correct = logical()
  )
})

test_that("graded() returns correct, incorrect, neutral", {
  # correct
  expect_graded(
    graded(TRUE, I("test")),
    TRUE,
    "test"
  )
  # Allows additional arguments or data in the graded condition
  correct <- expect_graded(
    graded(TRUE, I("test"), type = "info", location = "prepend"),
    TRUE,
    "test"
  )
  expect_equal(correct$type, "info")
  expect_equal(correct$location, "prepend")

  # incorrect
  expect_graded(graded(FALSE, I("test")), FALSE, "test")
  incorrect <- expect_graded(
    graded(FALSE, I("test"), type = "warning", location = "replace"),
    FALSE,
    "test"
  )
  expect_equal(incorrect$type, "warning")
  expect_equal(incorrect$location, "replace")

  # neutral
  expect_graded(graded(logical(), I("test")), logical(), "test")
  neutral <- expect_graded(
    graded(logical(), I("test"), type = "custom", location = "append"),
    logical(),
    "test"
  )
  expect_equal(neutral$type, "custom")
  expect_equal(neutral$location, "append")

  expect_error(graded("boom", I("bad")))
})

test_that("graded() passes along extra information in the ...", {
  expect_equal(graded(TRUE, "foo", arg = "boom!")$arg, "boom!")
  expect_equal(graded(TRUE, "foo", prop = list(a = "apple"))$prop, list(a = "apple"))
  expect_equal(pass("msg", prop = 42)$prop, 42)
  expect_equal(fail("msg", prop = 42)$prop, 42)

  gradethis_env <- rlang::env(".__gradethis_check_env" = TRUE)
  expect_equal(pass_if(TRUE, "msg", prop = 42, env = gradethis_env)$prop, 42)
  expect_equal(fail_if(TRUE, "msg", prop = 42, env = gradethis_env)$prop, 42)

  expect_equal(pass_if_equal(x = 1, y = 1, "msg", prop = 42)$prop, 42)
  expect_equal(fail_if_equal(x = 1, y = 1, "msg", prop = 42)$prop, 42)
  expect_equal(fail_if_code_feedback("msg", "a", "b", prop = 42)$prop, 42)

  # ... need to be named
  expect_error(graded(TRUE, "foo", "boom!"))
  # ... need to be unique
  expect_error(graded(TRUE, "foo", prop = 2, prop = 3))
})

test_that("pass_if() and fail_if() use default pass/fail message in grade_this()", {
  with_gradethis_setup(
    pass = "TEST PASSED",
    fail = "TEST FAILED",
    expect_grade_this({
      pass_if(.result < 5)
      fail_if(.result >= 5)
      fail("TEST FAILED")
    },
    user_code = "2",
    is_correct = TRUE,
    msg = "TEST PASSED"
    )
  )

  with_gradethis_setup(
    pass = "TEST FAILED",
    fail = "TEST PASSED",
    expect_grade_this({
      pass_if(.result < 5)
      fail_if(.result >= 5)
      fail("TEST FAILED")
    },
    user_code = "6",
    is_correct = FALSE,
    msg = "TEST PASSED"
    )
  )

  with_gradethis_setup(
    pass = "TEST FAILED",
    fail = "TEST PASSED.{maybe_code_feedback()}",
    expect_match(
      expect_grade_this({
        pass_if(.result < 5)
        fail_if(.result >= 5)
        fail("TEST FAILED")
      },
      user_code = "6",
      solution_code = "2",
      is_correct = FALSE,
      )$message,
      "TEST PASSED\\. I expected"
    )
  )
})

test_that("grade_if_equal() edge cases with diffobj::ses()", {
  result <- c(39.6, 40.1, 35, 42, 34.5, 41.4, 39, 40.6, 36.5, 37.6, 35.7,
    41.3, 37.6, 41.1, 36.4, 41.6, 35.5, 41.1, 35.9, 41.8, 33.5, 39.7,
    39.6, 45.8, 35.5, 42.8, 40.9, 37.2, 36.2, 42.1, 34.6, 42.9, 36.7,
    35.1, 37.3, 41.3, 36.3, 36.9, 38.3, 38.9, 35.7, 41.1, 34, 39.6,
    36.2, 40.8, 38.1, 40.3, 33.1, 43.2, 49.1, 48.4, 42.6, 44.4, 44,
    48.7, 42.7, 49.6, 45.3, 49.6, 50.5, 43.6, 45.5, 50.5, 44.9, 45.2,
    46.6, 48.5, 45.1, 50.1, 46.5, 45, 43.8, 45.5, 43.2, 50.4, 45.3,
    46.2, 45.7, 54.3, 45.8, 49.8, 46.2, 49.5, 43.5, 50.7, 47.7, 46.4,
    48.2, 46.5, 46.4, 48.6, 47.5, 51.1, 45.2, 45.2, 50.5, 49.5, 46.4,
    52.8, 40.9, 54.2, 42.5, 51, 49.7, 47.5, 47.6, 52, 46.9, 53.5,
    49, 46.2, 50.9, 45.5)

  solution <- c(39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, 42, 37.8,
    37.8, 41.1, 38.6, 34.6, 36.6, 38.7, 42.5, 34.4, 46, 37.8, 37.7,
    35.9, 38.2, 38.8, 35.3, 40.6, 40.5, 37.9, 40.5, 39.5, 37.2, 39.5,
    40.9, 36.4, 39.2, 38.8, 42.2, 37.6, 39.8, 36.5, 40.8, 36, 44.1,
    37, 39.6, 41.1, 37.5, 36, 42.3, 46.1, 50, 48.7, 50, 47.6, 46.5,
    45.4, 46.7, 43.3, 46.8, 40.9, 49, 45.5, 48.4, 45.8, 49.3, 42,
    49.2, 46.2, 48.7, 50.2, 45.1, 46.5, 46.3, 42.9, 46.1, 44.5, 47.8,
    48.2, 50, 47.3, 42.8, 45.1, 59.6, 49.6, 50.5, 50.5, 50.1, 50.4,
    46.2, 54.3, 49.8, 49.5, 50.7, 46.4, 48.2, 48.6, 45.2, 52.5, 50,
    50.8, 52.1, 52.2, 49.5, 50.8, 46.9, 51.1, 55.9, 49.1, 49.8, 51.5,
    55.1, 48.8, 50.4, 46.5, 50, 51.3, 45.4, 52.7, 45.2, 46.1, 51.3,
    46, 51.3, 46.6, 51.7, 47, 52, 45.9, 50.5, 50.3, 58, 46.4, 49.2,
    42.4, 48.5, 43.2, 50.6, 46.7, 52)

  # Logic Error: Exceeded buffer for finding fake snake; contact maintainer.
  expect_null(
    grade_if_equal(result, solution, message = "TEST FAILED", FALSE)
  )

  a <- 1:20
  b <- 10:500
  # Internal Error: reached theoretically unreachable branch 2, contact maintainer.
  expect_null(
    grade_if_equal(a, b, message = "TEST_FAILED", FALSE)
  )
})

test_that("praise argument works with passing grades", {
  with_seed(
    seed = 33,
    expect_graded(
      pass("xxx", praise = TRUE),
      is_correct = TRUE,
      msg = paste(with_seed(33, random_praise()), "xxx")
    )
  )

  with_seed(
    seed = 12,
    expect_graded(
      pass_if_equal(x = 1, y = 1, message = "xxx", praise = TRUE),
      is_correct = TRUE,
      msg = paste(with_seed(12, random_praise()), "xxx")
    )
  )

  with_options(
    list(gradethis.pass.praise = TRUE),
    with_seed(
      seed = 99,
      expect_graded(
        pass(message = "xxx"),
        is_correct = TRUE,
        msg = paste(with_seed(99, random_praise()), "xxx")
      )
    )
  )

  gradethis_env <- rlang::env(".__gradethis_check_env" = TRUE)

  # only one random_praise(), praise = TRUE wins
  with_seed(
    seed = 84,
    expect_graded(
      pass_if(TRUE, message = "{random_praise()}", praise = TRUE, env = gradethis_env),
      is_correct = TRUE,
      msg = with_seed(84, random_praise())
    )
  )
})

test_that("encourage argument works with failing grades", {
  with_seed(
    seed = 33,
    expect_graded(
      fail("xxx", encourage = TRUE),
      is_correct = FALSE,
      msg = paste("xxx", with_seed(33, random_encouragement()))
    )
  )

  with_seed(
    seed = 12,
    expect_graded(
      fail_if_equal(x = 1, y = 1, message = "xxx", encourage = TRUE),
      is_correct = FALSE,
      msg = paste("xxx", with_seed(12, random_encouragement()))
    )
  )

  with_seed(
    seed = 44,
    expect_graded(
      fail_if_code_feedback("1 + 1", "1 + 2", message = "xxx", encourage = TRUE),
      is_correct = FALSE,
      msg = paste(
        "xxx In `1 + 1`, I expected `2` where you wrote `1`.",
        with_seed(44, random_encouragement())
      )
    )
  )

  with_options(
    list(gradethis.fail.encourage = TRUE),
    with_seed(
      seed = 99,
      expect_graded(
        fail(message = "xxx"),
        is_correct = FALSE,
        msg = paste("xxx", with_seed(99, random_encouragement()))
      )
    )
  )

  gradethis_env <- rlang::env(".__gradethis_check_env" = TRUE)

  # only one random_encouragement(), encourage = TRUE wins
  with_seed(
    seed = 84,
    expect_graded(
      fail_if(TRUE, message = "{random_encouragement()}", encourage = TRUE, env = gradethis_env),
      is_correct = FALSE,
      msg = with_seed(84, random_encouragement())
    )
  )
})

test_that("errors in grade_this() are internal errors by default", {
  ex <- mock_this_exercise("'4'")

  # by default, errors are now turned into internal errors
  grade <- testthat::expect_message(
    expect_graded(
      grade_this(stop("boom"))(ex),
      is_correct = logical(),
      msg = "problem occurred"
    ),
    "#> grade_this\\("
  )
  expect_equal(grade$error$message, "boom")

  # without fail_if_error() errors become internal problem grades
  with_options(list(warn = -1), {
    grade_invalid <-
      testthat::expect_message(
        expect_graded(
          grade_this(runif("boom"))(ex),
          is_correct = logical(),
          msg = "problem occurred"
        ),
        "#> grade_this\\("
      )
    err_invalid <- tryCatch(runif("boom"), error = identity)
    expect_equal(grade_invalid$error$message, err_invalid$message)
    expect_equal(grade_invalid$error$call, deparse(err_invalid$call))

    grade_syntax <-
      testthat::expect_message(
        expect_graded(
          grade_this(eval(parse(text = "runif(")))(ex),
          is_correct = logical(),
          msg = "problem occurred"
        ),
        "#> grade_this\\("
      )
    err_syntax <- tryCatch(eval(parse(text = "runif(")), error = identity)
    expect_equal(grade_syntax$error$message, err_syntax$message)
    expect_equal(grade_syntax$error$call, deparse(err_syntax$call))
  })
})

test_that("errors in fail_if_error() become fail() grades", {
  ex <- mock_this_exercise("'4'")

  expect_graded(
    grade_this({
      fail_if_error(stop("boom"))
    })(ex),
    is_correct = FALSE,
    msg = "boom"
  )

  expect_graded(
    grade_this({
      fail_if_error({
        expect_length(.result, 1)
        expect_true(is.numeric(.result))
        expect_equal(.result, 4)
      })
      pass("Good job!")
    })(ex),
    is_correct = FALSE,
    msg = "is not TRUE"
  )

  expect_graded(
    grade_this({
      fail_if_error(
        message = "Your result isn't a single numeric value.",
        {
          testthat::expect_length(.result, 1)
          testthat::expect_true(is.numeric(.result))
          testthat::expect_equal(.result, 4)
        }
      )
      pass("Good job!")
    })(ex),
    is_correct = FALSE,
    msg = "Your result isn't a single numeric value."
  )
})

test_that("extra phrases aren't duplicated", {
  local_edition(2)
  local_mock(
    random_encouragement = function() "RANDOM ENCOURAGEMENT.",
    random_praise = function() "RANDOM PRAISE."
  )

  with_gradethis_setup(
    fail.encourage = TRUE,
    pass.praise = TRUE,
    fail = "\n\nDEFAULT FAIL MESSAGE. {random_encouragement()}",
    {
      grader <- grade_this({
        fail_if(
          identical(.result, 43),
          "SPECIFIC FAIL FEEDBACK",
          encourage = TRUE
        )
        pass_if(
          identical(.result, 42),
          "SPECIFIC PASS FEEDBACK",
          praise = TRUE
        )
      })

      grade_fail <- grader(mock_this_exercise(.user_code = 43, .solution_code = 42))
      grade_pass <- grader(mock_this_exercise(.user_code = 42, .solution_code = 42))
    }
  )

  expect_match_count <- function(text, pattern, n) {
    count <- length(strsplit(text, pattern)[[1]]) - 1
    expect_equal(!!count, !!n)
  }

  expect_match_count(grade_fail$message, "RANDOM ENCOURAGEMENT", 1L)
  expect_match_count(grade_pass$message, "RANDOM PRAISE", 1L)
})
