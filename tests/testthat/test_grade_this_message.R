context("Check grade_conditions messages")

test_that("Correct messages without random praise", {

  with_options(
    list(gradethis.pass = "A pass message"),
    {

      expect_grade_this(
        expr = {
          pass_if_equal(5, "A pass_if_equal message")
          fail()
        },
        user_code = "5",
        is_correct = TRUE,
        msg = "A pass_if_equal message"
      )

      expect_grade_this(
        expr = {
          pass_if_equal(5)
          fail()
        },
        user_code = "5",
        is_correct = TRUE,
        msg = "A pass message"
      )

    }
  )

})

test_that("Incorrect messages no match pass_if", {
  with_options(
    list(gradethis.fail = "A fail message"),
    {
      expect_grade_this(
        expr = {
          fail_if_equal(5, "A fail_if_equal message")
          pass()
        },
        user_code = "5",
        is_correct = FALSE,
        msg = "A fail_if_equal message"
      )

      expect_grade_this(
        expr = {
          fail_if_equal(5)
          pass()
        },
        user_code = "5",
        is_correct = FALSE,
        msg = "A fail message"
      )
    }
  )

})


test_that("messages work with glue", {
  with_options(
    list(
      gradethis.pass = "A pass message. {extra}",
      gradethis.fail = "A fail message. {extra}"
    ),
    {

      expect_grade_this(
        expr = {
          extra <- "Extra!"
          pass_if_equal(5, "A pass_if_equal message. {extra}")
          fail()
        },
        user_code = "5",
        is_correct = TRUE,
        msg = "A pass_if_equal message. Extra!"
      )

      expect_grade_this(
        expr = {
          extra <- "Extra!"
          pass_if_equal(5)
          fail()
        },
        user_code = "5",
        is_correct = TRUE,
        msg = "A pass message. Extra!"
      )

      expect_grade_this(
        expr = {
          extra <- "Extra!"
          fail_if_equal(5, "A fail_if_equal message. {extra}")
          pass()
        },
        user_code = "5",
        is_correct = FALSE,
        msg = "A fail_if_equal message. Extra!"
      )

      expect_grade_this(
        expr = {
          extra <- "Extra!"
          fail_if_equal(5)
          pass()
        },
        user_code = "5",
        is_correct = FALSE,
        msg = "A fail message. Extra!"
      )
    }
  )

})
