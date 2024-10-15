test_that("General utility tests", {

    expect_true(isValidColor("#FFF000"))
    expect_true(isValidColor("green"))
    expect_true(isValidColor("#fff"))
    expect_true(isValidColor("#FFF"))
    expect_true(isValidColor("#234"))
    expect_true(isValidColor("#111111"))
    expect_false(isValidColor("greeeen"))
    expect_false(isValidColor("#III234"))
    expect_false(isValidColor("#FFFFFFF"))
    expect_false(isValidColor("#fffffff"))

})


