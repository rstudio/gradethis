context("Check grade_conditions messages")

test_that("Correct messages without random praise", {

  with_options(
    list(gradethis.pass = "A pass message"),
    {

      eval_this(
        expr = {
          pass_if_equal(5, "A pass_if_equal message")
          fail()
        },
        user_code = "5"
      ) %>%
        expect_correct() %>%
        expect_message("A pass_if_equal message")

      eval_this(
        expr = {
          pass_if_equal(5)
          fail()
        },
        user_code = "5"
      ) %>%
        expect_correct() %>%
        expect_message("A pass message")

    }
  )

})

test_that("Incorrect messages no match pass_if", {
  with_options(
    list(gradethis.fail = "A fail message"),
    {
      eval_this(
        expr = {
          fail_if_equal(5, "A fail_if_equal message")
          pass()
        },
        user_code = "5"
      ) %>%
        expect_wrong() %>%
        expect_message("A fail_if_equal message")

      eval_this(
        expr = {
          fail_if_equal(5)
          pass()
        },
        user_code = "5"
      ) %>%
        expect_wrong() %>%
        expect_message("A fail message")
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

      eval_this(
        expr = {
          extra <- "Extra!"
          pass_if_equal(5, "A pass_if_equal message. {extra}")
          fail()
        },
        user_code = "5"
      ) %>%
        expect_correct() %>%
        expect_message("A pass_if_equal message. Extra!")

      eval_this(
        expr = {
          extra <- "Extra!"
          pass_if_equal(5)
          fail()
        },
        user_code = "5"
      ) %>%
        expect_correct() %>%
        expect_message("A pass message. Extra!")

      eval_this(
        expr = {
          extra <- "Extra!"
          fail_if_equal(5, "A fail_if_equal message. {extra}")
          pass()
        },
        user_code = "5"
      ) %>%
        expect_wrong() %>%
        expect_message("A fail_if_equal message. Extra!")

      eval_this(
        expr = {
          extra <- "Extra!"
          fail_if_equal(5)
          pass()
        },
        user_code = "5"
      ) %>%
        expect_wrong() %>%
        expect_message("A fail message. Extra!")
    }
  )

})
