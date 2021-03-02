test_that("random_praise() returns a string", {
  expect_equal(length(random_praise()), 1L)
  expect_type(random_praise(), "character")
})

test_that("random_encouragement() returns a string", {
  expect_equal(length(random_encouragement()), 1L)
  expect_type(random_encouragement(), "character")
})


with_seed <- function(seed, expr) {
  set.seed(seed)
  expr
}

test_that("give_praise() gives praise", {
  expect_equal(
    give_praise("xxx"),
    "{random_praise()} xxx"
  )
  
  expect_equal(
    give_praise("xxx", after = ""),
    "{random_praise()}xxx"
  )
  
  expect_equal(
    give_praise("xxx", location = "after"),
    "xxx {random_praise()}"
  )
  
  expect_equal(
    give_praise("xxx", location = "after", before = ""),
    "xxx{random_praise()}"
  )
  
  expect_equal(
    give_praise("", before = "__", after = "--"),
    "__{random_praise()}--"
  )
  
  expect_equal(
    with_seed(42, give_praise(pass(""), after = "")$message),
    with_seed(42, random_praise())
  )
  
  expect_equal(give_praise(fail("none"))$message, "none")
  
  expect_equal(
    with_seed(43, give_praise(function(...) pass(""), after = "")(NULL)$message),
    with_seed(43, random_praise())
  )
  
  expect_equal(
    with_praise(FALSE, random_praise()),
    ""
  )
  
  expect_true(with_praise(TRUE, getOption("gradethis.__praise")))
  expect_null(getOption("gradethis.__praise", NULL))
  expect_false(with_praise(FALSE, getOption("gradethis.__praise")))
  
  expect_error(
    give_praise("", location = "boom")
  )
  
  expect_null(give_praise(NULL))
  
  expect_error(give_praise(12))
  
  expect_error(give_praise("", foo = 2), "foo")
})

test_that("give_encouragement() gives encouragemnt", {
  expect_equal(
    give_encouragement("xxx"),
    "xxx {random_encouragement()}"
  )
  
  expect_equal(
    give_encouragement("xxx", before = ""),
    "xxx{random_encouragement()}"
  )
  
  expect_equal(
    give_encouragement("xxx", location = "before"),
    "{random_encouragement()} xxx"
  )
  
  expect_equal(
    give_encouragement("xxx", location = "before", after = ""),
    "{random_encouragement()}xxx"
  )
  
  expect_equal(
    give_encouragement("", before = "__", after = "--"),
    "__{random_encouragement()}--"
  )
  
  expect_equal(
    with_seed(42, give_encouragement(fail(""), before = "")$message),
    with_seed(42, random_encouragement())
  )
  
  expect_equal(give_encouragement(pass("none"))$message, "none")
  
  expect_equal(
    with_seed(43, give_encouragement(function(...) fail(""), before = "")(NULL)$message),
    with_seed(43, random_encouragement())
  )
  
  expect_equal(
    with_encouragement(FALSE, random_encouragement()),
    ""
  )
  
  expect_true(with_encouragement(TRUE, getOption("gradethis.__encouragement")))
  expect_null(getOption("gradethis.__encouragement", NULL))
  expect_false(with_encouragement(FALSE, getOption("gradethis.__encouragement")))
  
  expect_error(
    give_encouragement("", location = "boom")
  )
  
  expect_null(give_encouragement(NULL))
  
  expect_error(give_encouragement(12))
  
  expect_error(give_encouragement("", foo = 2), "foo")
})
