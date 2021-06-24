
test_that("markdown utilities: is_tag_like", {
  expect_true(is_tag_like(htmltools::p(htmltools::strong("a"), htmltools::em("b"))))
  expect_true(is_tag_like(htmltools::tagList(htmltools::p("1"), htmltools::p("2"))))
  expect_false(is_tag_like("a"))
  expect_false(is_tag_like(htmltools::HTML("a")))
  expect_false(is_tag_like(I("a")))
})

test_that("markdown utilities: is_AsIs", {
  expect_true(is_AsIs(I("x")))
  expect_true(is_AsIs(I(c("x", "y"))))
  expect_false(is_AsIs(c("x", "y")))
})

test_that("markdown: message_md() returns HTML", {
  md_html <- message_md(c("_It_", "**Works!**"))
  
  expect_s3_class(md_html, "html")
  expect_type(md_html, "character")
  expect_equal(length(md_html), 1)
  expect_equal(md_html, htmltools::HTML("<p><em>It</em>\n<strong>Works!</strong></p>\n"))
})

test_that("markdown: HTML tags and tag lists are returned untouched", {
  tag <- htmltools::p("{one} tag")
  tag_list <- htmltools::tagList(htmltools::p("{one}"), htmltools::p("{two}"))
  env <- new.env()
  env$one <- "1"
  env$two <- "2"
  
  expect_equal(glue_message_with_env(env, tag), tag)
  expect_equal(glue_message(tag, one = "1"), tag)
  expect_equal(message_md(tag), tag)
  
  expect_equal(glue_message_with_env(env, tag_list), tag_list)
  expect_equal(glue_message(tag_list, one = "1", two = "2"), tag_list)
  expect_equal(message_md(tag_list), tag_list)
})

test_that("markdown: AsIs text is returned untouched", {
  txt <- I("__{one}__ <em>{two}</em>")
  env <- new.env()
  env$one <- "1"
  env$two <- "2"
  
  expect_equal(glue_message_with_env(env, txt), txt)
  expect_equal(glue_message(txt, one = "1", two = "2"), txt)
  expect_equal(message_md(txt), "__{one}__ <em>{two}</em>")
})

test_that("markdown: disallowed tags are escaped", {
  expect_match(message_md("<script>alert('boo')</script>"), "&lt;script")
  expect_match(message_md("<style></style>"), "&lt;style")
  expect_match(message_md(I("<style></style>")), "<style></style>")
})

test_that("markdown: grading functions handle HTML messages", {
  # grade_this() ----
  expect_grade_this(
    pass(htmltools::strong("Great")),
    user_code = "1 + 1", 
    solution_code = "1 + 1",
    is_correct = TRUE, 
    msg = htmltools::strong("Great")
  )
  expect_grade_this(
    pass(htmltools::HTML("<strong>Great</strong>")),
    user_code = "1 + 1", 
    solution_code = "1 + 1", 
    is_correct = TRUE, 
    msg = "<strong>Great</strong>"
  )
  
  # grade_this_code() ----
  expect_this_code(
    user_code = "1 + 1", 
    solution_code = "1 + 1",
    correct = htmltools::strong("Great"),
    is_correct = TRUE, 
    msg = htmltools::strong("Great")
  )
  expect_this_code(
    user_code = "1 + 1", 
    solution_code = "1 + 1", 
    correct = htmltools::HTML("<strong>Great</strong>"),
    is_correct = TRUE, 
    msg = "<strong>Great</strong>"
  )
  expect_this_code(
    user_code = "1 + 2", 
    solution_code = "1 + 1",
    incorrect = htmltools::strong("Nope"),
    is_correct = FALSE, 
    msg =  htmltools::strong("Nope")
  )
  expect_this_code(
    user_code = "1 + 2", 
    solution_code = "1 + 1", 
    incorrect = htmltools::HTML("<strong>Nope</strong>"),
    is_correct = FALSE, 
    msg = "<strong>Nope</strong>"
  )
  
  # grade_code() ----
  ## correct message starts with praise so HTML is rendered down to character
  expect_grade_code(
    correct = htmltools::strong("Great"),
    user_code = "1 + 1", 
    solution_code = "1 + 1", 
    is_correct = TRUE, 
    msg = "<strong>Great</strong>"
  )
  expect_grade_code(
    correct = htmltools::HTML("<strong>Great</strong>"),
    user_code = "1 + 1", 
    solution_code = "1 + 1", 
    is_correct = TRUE, 
    msg = "<strong>Great</strong>"
  )
  
  # grade_result() ----
  ## fail message ends with encouragement so HTML is rendered down to character
  expect_grade_result(
    fail_if(~ .result == 2, htmltools::code("2")),
    last_value = 1 + 1, 
    is_correct = FALSE,
    msg = "<code>2</code>"
  )
  expect_grade_result(
    pass_if(~ .result == 2, htmltools::HTML("<code>2</code>")),
    last_value = 1 + 1, 
    is_correct = TRUE, 
    msg = "<code>2</code>"
  )
})

test_that("feedback() converts graded() to feedback", {
  # correct
  expect_feedback(
    graded(TRUE, I("test")),
    is_correct = TRUE,
    type = "success",
    location = "append",
    msg = "test"
  )

  # incorrect
  expect_feedback(
    graded(FALSE, I("test")),
    is_correct = FALSE,
    type = "error",
    location = "append",
    msg = "test"
  )
  
  # neutral
  expect_feedback(
    graded(logical(0), I("test")),
    is_correct = logical(0),
    type = "custom",
    location = "append",
    msg = "test"
  )
})

test_that("feedback() uses graded type and location", {
  # correct
  expect_feedback(
    graded(TRUE, I("test"), type = "info"),
    is_correct = TRUE,
    type = "info",
    location = "append",
    msg = "test"
  )
  
  # incorrect
  expect_feedback(
    graded(FALSE, I("test"), type = "warning", location = "prepend"),
    is_correct = FALSE,
    type = "warning",
    location = "prepend",
    msg = "test"
  )
  
  # neutral
  expect_feedback(
    graded(logical(0), I("test"), type = "auto", location = "replace"),
    is_correct = logical(0),
    type = "custom",
    location = "replace",
    msg = "test"
  )
})

test_that("feedback() prefers graded options over feedback options", {
  # correct
  expect_feedback(
    feedback(
      graded(TRUE, I("test"), type = "info"), 
      type = "success"
    ),
    is_correct = TRUE,
    type = "info",
    location = "append",
    msg = "test"
  )
  
  # incorrect
  expect_feedback(
    feedback(
      graded(FALSE, I("test"), type = "warning", location = "prepend"),
      type = "error",
      location = "append"
    ),
    is_correct = FALSE,
    type = "warning",
    location = "prepend",
    msg = "test"
  )
  
  # neutral
  expect_feedback(
    feedback(
      graded(logical(0), I("test"), type = "auto", location = "replace"),
      type = "info",
      location = "append"
    ),
    is_correct = logical(0),
    type = "custom",
    location = "replace",
    msg = "test"
  )
})

test_that("feedback() passes along extra information in the from graded()", {
  expect_equal(feedback(graded(TRUE, "foo", arg = "boom!"))$arg, "boom!")
  expect_equal(feedback(graded(TRUE, "foo", prop = list(a = "apple")))$prop, list(a = "apple"))
  expect_equal(feedback(pass("msg", prop = 42))$prop, 42)
  expect_equal(feedback(fail("msg", prop = 42))$prop, 42)
  
  gradethis_env <- rlang::env(".__gradethis_check_env" = TRUE)
  expect_equal(feedback(pass_if(TRUE, "msg", prop = 42, env = gradethis_env))$prop, 42)
  expect_equal(feedback(fail_if(TRUE, "msg", prop = 42, env = gradethis_env))$prop, 42)
  
  expect_equal(feedback(pass_if_equal(x = 1, y = 1, "msg", prop = 42))$prop, 42)
  expect_equal(feedback(fail_if_equal(x = 1, y = 1, "msg", prop = 42))$prop, 42)
  expect_equal(feedback(fail_if_code_feedback("msg", "a", "b", prop = 42))$prop, 42)
  
  # ... need to be named (if we somehow get around checks in graded())
  grade <- graded(TRUE, "foo", foo = "boom!")
  names(grade)[5] <- NA_character_
  expect_error(feedback(grade))
  names(grade)[5] <- ""
  expect_error(feedback(grade))
  
  # ... need to be unique (if we somehow get around checks in graded())
  grade <- graded(TRUE, "foo", prop = 2, prop2 = 3)
  names(grade)[6] <- "prop"
  expect_error(feedback(grade))
})

