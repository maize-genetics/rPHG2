test_that("General metrics utility tests", {
    # validateHeaders()
    x <- "some\tline\tto\ttest"
    expect_true(
        rPHG2:::validateHeaders(x, c("some", "line", "to", "test"))
    )
    expect_false(
        rPHG2:::validateHeaders(NULL, c("some", "line", "to", "test"))
    )
})


