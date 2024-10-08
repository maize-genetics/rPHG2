test_that("JvmStats class construction tests", {

    jvmTest <- jvmStats()

    expect_true(is(javaVersion(jvmTest), "character"))
    expect_true(is(phgVersion(jvmTest), "character"))
    expect_true(is(phgVersion(jvmTest, granular = TRUE), "list"))
    expect_true(is(classPath(jvmTest), "character"))
    expect_true(is(jvmMemStats(jvmTest), "list"))


    locConOutput <- utils::capture.output(jvmTest)
    expect_equal(length(locConOutput), 10)
})


