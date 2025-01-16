## ----
# Mock conda installation
test_that("createMockCondaInstallation creates expected structure", {
    # Use a temporary directory for testing
    tmpDir <- file.path(tempdir(), "mock_conda_test")
    on.exit(unlink(tmpDir, recursive = TRUE, force = TRUE), add = TRUE)

    # Call the function
    createMockCondaInstallation(
        baseDir       = tmpDir,
        condaEnvName  = "test-env",
        verbose       = FALSE
    )

    # Check that the base directory exists
    expect_true(dir.exists(tmpDir))

    # Check that bin folder exists
    expect_true(dir.exists(file.path(tmpDir, "bin")))

    # Check that envs folder and test-env subfolder exist
    testEnvDir <- file.path(tmpDir, "envs", "test-env")
    expect_true(dir.exists(file.path(tmpDir, "envs")))
    expect_true(dir.exists(testEnvDir))

    # Check that environment subdirectories exist
    expect_true(dir.exists(file.path(testEnvDir, "bin")))
    expect_true(dir.exists(file.path(testEnvDir, "lib")))
    expect_true(dir.exists(file.path(testEnvDir, "include")))
    expect_true(dir.exists(file.path(testEnvDir, "share")))
})


