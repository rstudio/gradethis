
test_that("markdown: message_md() handles text and HTML", {
  expect_equal(
    message_md("<strong>Works!</strong>"),
    message_md(htmltools::strong("Works!"))
  )
})

test_that("markdown: disallowed tags are escaped", {
  expect_match(message_md("<script>alert('boo')</script>"), "&lt;script")
  expect_match(message_md(htmltools::tags$script("alert('boo')")), "&lt;script")
  
  expect_match(message_md("<style></style>"), "&lt;style")
  expect_match(message_md(htmltools::tags$style("")), "&lt;style")
})

test_that("markdown: grading functions handle HTML messages", {
  # grade_this() ----
  expect_grade_this(
    pass(htmltools::strong("Great")),
    user_code = "1 + 1", 
    solution_code = "1 + 1",
    is_correct = TRUE, 
    msg = "<strong>Great</strong>"
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
    msg = "<strong>Great</strong>"
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
    msg = "<strong>Nope</strong>"
  )
  expect_this_code(
    user_code = "1 + 2", 
    solution_code = "1 + 1", 
    incorrect = htmltools::HTML("<strong>Nope</strong>"),
    is_correct = FALSE, 
    msg = "<strong>Nope</strong>"
  )
  
  # grade_code() ----
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
  expect_grade_result(
    pass_if(~ .result == 2, htmltools::code("2")),
    last_value = 1 + 1, 
    is_correct = TRUE, 
    msg = "<code>2</code>"
  )
  expect_grade_result(
    pass_if(~ .result == 2, htmltools::HTML("<code>2</code>")),
    last_value = 1 + 1, 
    is_correct = TRUE, 
    msg = "<code>2</code>"
  )
})
