test_that("PHG initialization", {
    expect_error(initPhg("dir"))
    expect_error(initPhg(mtcars))
})


