context("Check sanity messages")

a <<- function(x) x
b <<- function(x) x
f <<- function(x, y) x + y
g <<- function(x, y = a(1)) x + y

test_that("detect_mistakes detects wrong calls", {
  
  solution <- quote(b(1))
  user <-     quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = quote(a()), that = quote(b()))
  )
  
  solution <- quote(a())
  user <-     quote(a)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(this = quote(a), that = quote(a()))
  )
  
  solution <- quote(a(1))
  user <-     quote(b(a(1)))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = user, that = solution)
  )
  
  solution <- quote(a(1))
  user <-     quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(this = user[[2]], that = solution[[2]], enclosing_call = user)
  )
  
  solution <- quote(a(1))
  user <-     quote(a(a(1)))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(this = user[[2]], that = solution[[2]], enclosing_call = user)
  )
  
  solution <- quote(a(1))
  user <-     quote(0 + a(1))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = user, that = solution)
  )
  
  solution <- quote(a(1))
  user <-     quote(a(1) + 0)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = user, that = solution)
  )
  
  solution <- quote(a() + b())
  user <-     quote(a() + a())
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = user[[3]], that = solution[[3]], enclosing_call = user)
  )
  
  solution <- quote(a() + b())
  user <-     quote(b() + a())
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = user[[2]], that = solution[[2]], enclosing_call = user)
  )
  
  solution <- quote(a(b(1)))
  user <-     quote(b(1) %>% a())
  expect_null(
    detect_mistakes(user, solution)
  )
  
  solution <- quote(a(b(1)))
  user <-     quote(a(1) %>% b())
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = user[[3]], that = solution)
  )
  
  solution <- quote(a(b(1)))
  user <-     quote(a(1) %>% a())
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = user[[3]], that = solution[[2]], enclosing_call = user)
  )
  
  solution <- quote(a(b(1)))
  user <-     quote(b(1) %>% b())
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(this = user[[3]], that = solution)
  )
  
  solution <- quote(f(1, y = a(1)))
  user <-     quote(f(1, y = b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(
      this = user[[3]], 
      that = solution[[3]], 
      this_name = "y", 
      enclosing_call = user
    )
  )
  
  solution <- quote(f(1, y = a(1)))
  user <-     quote(f(1, y = b(a(1))))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(
      this = user[[3]], 
      that = solution[[3]], 
      this_name = "y", 
      enclosing_call = user
    )
  )
  
  solution <- quote(f(1, y = a(1)))
  user <-     quote(f(1, y = a(b(1))))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user[[3]][[2]], 
      that = solution[[3]][[2]], 
      enclosing_call = user[[3]]
    )
  )
  
  solution <- quote(f(1, y = a(1)))
  user <-     quote(f(1, y = b(b(1))))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(
      this = user[[3]], 
      that = solution[[3]], 
      this_name = "y", 
      enclosing_call = user
    )
  )
  
  solution <- quote(f(1, y = a(1)))
  user <-     quote(f(1, y = a(a(1))))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user[[3]][[2]], 
      that = solution[[3]][[2]], 
      enclosing_call = user[[3]]
    )
  )
  
  solution <- quote(g(1))
  user <-     quote(g(1, y = a(1)))
  expect_null(
    detect_mistakes(user, solution)
  )
  
  solution <- quote(g(1))
  user <-     quote(g(1, y = b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(
      this = user[[3]], 
      that = formals(g)[[2]], 
      this_name = "y", 
      enclosing_call = user
    )
  )
  
  solution <- quote(f(1, y = a(1)))
  user <-     quote(f(1, y = f(1)))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_call(
      this = user[[3]], 
      that = formals(g)[[2]], 
      this_name = "y", 
      enclosing_call = user
    )
  )
  
})

test_that("detect_mistakes detects wrong values", {
  
  x <<- 1
  y <<- 1
  X <<- 1
  
  solution <- quote(1)
  user <-     quote(2)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user, 
      that = solution
    )
  )
  
  solution <- quote(1)
  user <-     quote(-1)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user, 
      that = solution
    )
  )
  
  solution <- quote(1)
  user <-     quote(1L)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user, 
      that = solution
    )
  )
  
  solution <- quote(1)
  user <-     quote(1())
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user, 
      that = solution
    )
  )
  
  solution <- quote(x)
  user <-     quote(y)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user, 
      that = solution
    )
  )
  
  solution <- quote(x)
  user <-     quote(X)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user, 
      that = solution
    )
  )
  
  solution <- quote(a(1))
  user <-     quote(a(2))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = deparse_to_string(user[[2]]), 
      that = solution[[2]], 
      enclosing_call = user
    )
  )
  
  solution <- quote(a(1))
  user <-     quote(a(-1))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = user[[2]], 
      that = solution[[2]], 
      enclosing_call = user
    )
  )
  
  solution <- quote(a(1))
  user <-     quote(a(1L))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = deparse_to_string(user[[2]]), 
      that = solution[[2]], 
      enclosing_call = user
    )
  )
  
  solution <- quote(a(x))
  user <-     quote(a(y))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = deparse_to_string(user[[2]]), 
      that = solution[[2]], 
      enclosing_call = user
    )
  )
  
  solution <- quote(a(x))
  user <-     quote(a(X))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = deparse_to_string(user[[2]]), 
      that = solution[[2]], 
      enclosing_call = user
    )
  )
  
  solution <- quote(1 + 2)
  user <-     quote(1 + 1)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = deparse_to_string(user[[3]]), 
      that = solution[[3]], 
      enclosing_call = user
    )
  )
  
  solution <- quote(1 + 2)
  user <-     quote(2 + 1)
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = deparse_to_string(user[[2]]), 
      that = solution[[2]], 
      enclosing_call = user
    )
  )
  
  solution <- quote(1 %>% f(2))
  user <-     quote(3 %>% f(2))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = deparse_to_string(user[[2]]), 
      that = solution[[2]], 
      enclosing_call = user
    )
  )
  
  solution <- quote(1 %>% f(2))
  user <-     quote(1 %>% f(3))
  expect_equal(
    detect_mistakes(user, solution),
    wrong_value(
      this = deparse_to_string(user[[3]][[2]]), 
      that = solution[[3]][[2]], 
      enclosing_call = user
    )
  )
  
})

test_that("detect_mistakes detects bad argument names", {
  
  tricky <<- function(aa = 1, ab = 2, ac = 3) aa
  
  solution <- quote(tricky(ab = 1))
  user <-     quote(tricky(a = 1))
  expect_equal(
    detect_mistakes(user, solution),
    bad_argument_name(
      this_call = user, 
      this = user[[2]], 
      this_name = names(as.list(user)[2])
    )
  )
  
  solution <- quote(tricky(ab = 1))
  user <-     quote(tricky(1, 2, a = 1))
  expect_equal(
    detect_mistakes(user, solution),
    bad_argument_name(
      this_call = user, 
      this = user[[4]], 
      this_name = names(as.list(user)[4])
    )
  )
  
  solution <- quote(tricky(ab = 1))
  user <-     quote(1 %>% tricky(a = 2))
  expect_equal(
    detect_mistakes(user, solution),
    bad_argument_name(
      this_call = user[[3]], 
      this = user[[3]][[2]], 
      this_name = names(as.list(user[[3]])[2])
    )
  )
  
  solution <- quote(tricky(ab = 1))
  user <-     quote(1 %>% tricky(a = .))
  expect_equal(
    detect_mistakes(user, solution),
    bad_argument_name(
      this_call = user[[3]], 
      this = user[[2]], 
      this_name = names(as.list(user[[3]])[2])
    )
  )
  
})

test_that("detect_mistakes detects too many matches", {
  
  tricky2 <<- function(ambiguous = 1, ...) ambiguous
  not_tricky <<- function(a = 1, ambiguous = 2, ...) a
  
  solution <- quote(tricky2(ambiguous = 2))
  user <-     quote(tricky2(a = 2, am = 2))
  expect_equal(
    detect_mistakes(user, solution),
    too_many_matches(
      this_call = user, 
      that_name = names(as.list(solution)[2])
    )
  )
  
  solution <- quote(not_tricky(ambiguous = 2))
  user <-     quote(not_tricky(a = 1, am = 2))
  expect_null(
    detect_mistakes(user, solution)
  )
  
  solution <- quote(tricky2(ambiguous = 2))
  user <-     quote(2 %>% tricky2(a = ., am = 2))
  expect_equal(
    detect_mistakes(user, solution),
    too_many_matches(
      this_call = user[[3]], 
      that_name = names(as.list(solution)[2])
    )
  )
  
})

test_that("detect_mistakes detects surplus arguments", {
  
  h <<- function(x) x
  i <<- function(x, ...) x
  
  solution <- quote(h(x = 1))
  user <-     quote(h(x = 1, y = 2))
  expect_equal(
    detect_mistakes(user, solution),
    surplus_argument(
      this_call = user, 
      this = user[[3]], 
      this_name = names(as.list(user)[3])
    )
  )
  
  solution <- quote(h(x = 1))
  user <-     quote(h(1, 2))
  expect_equal(
    detect_mistakes(user, solution),
    surplus_argument(
      this_call = user, 
      this = user[[3]], 
      this_name = names(as.list(user)[3])
    )
  )
  
  solution <- quote(i(x = 1))
  user <-     quote(i(1, 2))
  expect_equal(
    detect_mistakes(user, solution),
    surplus_argument(
      this_call = user, 
      this = user[[3]], 
      this_name = names(as.list(user)[3])
    )
  )
  
  solution <- quote(i(x = 1))
  user <-     quote(i(1, x = 2))
  expect_equal(
    detect_mistakes(user, solution),
    surplus_argument(
      this_call = user, 
      this = user[[2]], 
      this_name = names(as.list(user)[2])
    )
  )
  
  solution <- quote(h(x = 1))
  user <-     quote(1 %>% h(2))
  expect_equal(
    detect_mistakes(user, solution),
    surplus_argument(
      this_call = user[[3]], 
      this = user[[3]][[2]], 
      this_name = names(as.list(as.list(user)[[3]])[2])
    )
  )
  
})

test_that("detect_mistakes detects missing argument", {
  
  solution <- quote(f(x = 1, y = 1))
  user <-     quote(f(x = 1))
  expect_equal(
    detect_mistakes(user, solution),
    missing_argument(
      this_call = user, 
      that_name = names(as.list(solution)[3])
    )
  )
  
  solution <- quote(a(f(x = 1, y = 1)))
  user <-     quote(a(f(x = 1)))
  expect_equal(
    detect_mistakes(user, solution),
    missing_argument(
      this_call = user[[2]], 
      that_name = names(as.list(solution[[2]])[3]),
      enclosing_call = user
    )
  )
  
  solution <- quote(f(x = 1, y = 1))
  user <-     quote(f(y = 1))
  expect_equal(
    detect_mistakes(user, solution),
    missing_argument(
      this_call = user, 
      that_name = names(as.list(solution)[2])
    )
  )
  
  solution <- quote(f(x = 1, y = 1))
  user <-     quote(f(1))
  expect_equal(
    detect_mistakes(user, solution),
    missing_argument(
      this_call = user, 
      that_name = names(as.list(solution)[3])
    )
  )
  
  solution <- quote(f(x = 1, y = 1))
  user <-     quote(1 %>% f())
  expect_equal(
    detect_mistakes(user, solution),
    missing_argument(
      this_call = user[[3]], 
      that_name = names(as.list(solution)[3])
    )
  )
  
  solution <- quote(a(f(x = 1, y = 1)))
  user <-     quote(a(1 %>% f()))
  expect_equal(
    detect_mistakes(user, solution),
    missing_argument(
      this_call = user[[2]][[3]], 
      that_name = names(as.list(solution[[2]])[3]),
      enclosing_call = user
    )
  )
  
})
