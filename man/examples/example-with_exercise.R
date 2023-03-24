exercise <- mock_this_exercise(.user_code = "2", .solution_code = "1 + 1")

with_exercise(exercise, pass_if_equal())
with_exercise(exercise, fail_if_code_feedback())
