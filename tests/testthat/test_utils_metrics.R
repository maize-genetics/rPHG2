test_that("General metrics utility tests", {
    # validateHeaders()
    x <- "some\tline\tto\ttest"
    expect_true(
        validateHeaders(x, c("some", "line", "to", "test"))
    )
    expect_false(
        validateHeaders(NULL, c("some", "line", "to", "test"))
    )
    expect_false(
        validateHeaders(NULL, c("some", "line", "to", "ttest"))
    )
})


