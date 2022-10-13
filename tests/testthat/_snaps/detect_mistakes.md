# detect_mistakes works with infix operators

    Code
      detect_mistakes(quote(TRUE & FALSE), quote(TRUE | FALSE))
    Output
      I expected you to call `|` where you called `&`.

---

    Code
      detect_mistakes(quote(2^2), quote(2 * 2))
    Output
      I expected you to call `*` where you called `^`.

---

    Code
      detect_mistakes(quote(obj$value), quote(obj@value))
    Output
      I expected you to call `@` where you called `$`.

---

    Code
      detect_mistakes(quote(y <- m * x + b), quote(y ~ m * x + b))
    Output
      I expected you to call `~` where you called `<-`.

---

    Code
      detect_mistakes(quote(1 - 4), quote(1:4))
    Output
      I expected you to call `:` where you called `-`.

