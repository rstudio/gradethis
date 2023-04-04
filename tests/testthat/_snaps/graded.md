# pass_if() and fail_if() give errors for invalid cond

    Code
      grade <- expect_grade_this(pass_if(~TRUE), user_code = "1", solution_code = "2",
      is_correct = logical(0))
    Warning <simpleWarning>
      The `cond` argument to `pass_if()` does not accept functions or formulas when used inside `grade_this()`.

---

    Code
      grade <- expect_grade_this(fail_if(~TRUE), user_code = "1", solution_code = "2",
      is_correct = logical(0))
    Warning <simpleWarning>
      The `cond` argument to `fail_if()` does not accept functions or formulas when used inside `grade_this()`.

---

    Code
      grade <- expect_grade_this(pass_if(all.equal(.result, .solution)), user_code = "1",
      solution_code = "2", is_correct = logical(0))
    Warning <simpleWarning>
      The `cond` argument to `pass_if()` must be coercible to logical, not an object of class <character>.

---

    Code
      grade <- expect_grade_this(fail_if(!all.equal(.result, .solution)), user_code = "1",
      solution_code = "2", is_correct = logical(0))
    Warning <simpleWarning>
      The `cond` argument to `fail_if()` produced an error:
        Error in !all.equal(.result, .solution) : invalid argument type

