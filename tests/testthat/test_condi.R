context("Check condi")

expect_condi <- function(x) {
    checkmate::expect_names(names(x), identical.to = c("x", "message", "correct", "type"))
    checkmate::expect_character(x$message, null.ok = TRUE)
    checkmate::expect_logical(x$correct, null.ok = FALSE, len = 1)
    checkmate::expect_choice(x$type, choices = c("formula", "function", "value"))
    checkmate::expect_class(x, 'grader_condition')
}

condi_formula_t <- condi(~ .result == 5, message = 'my message', correct = TRUE)
condi_formula_f <- condi(~ .result == 5, message = 'my message', correct = FALSE)

test_that('check condi', {
    expect_condi(condi_formula_t)
    testthat::expect_true(rlang::is_formula(condi_formula_t$x))
})

context("Check evaluate_condi")

grader_args <- list(solution_quo = quote(5))
learnr_args <- list(envir_prep = new.env())

test_that('Condi switch statement', {
    # expect_true(
    #     evaluate_condi(condi_formula_t, grader_args, learnr_args)
    # )
})

context("Check condi_formula")

test_that('Condi formula', {
    expect_true(
        evaluate_condi_formula(~ .result == 5, user_answer = 5, env = new.env())
    )

    expect_true(
        evaluate_condi_formula(~ .result == 5,user_answer = grader_args$solution_quo, env = learnr_args$envir_prep)
    )

    expect_true(
        evaluate_condi_formula(~ . == 5, user_answer = 5, env = new.env())
    )

    expect_false(
        evaluate_condi_formula(~ .result == 5, user_answer = 4, env = new.env())
    )

    expect_false(
        evaluate_condi_formula(~ . == 5, user_answer = 4, env = new.env())
    )
})
