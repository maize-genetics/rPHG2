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


## ----
# Download
test_that("downloadAgcBinary constructs correct URL and unpacks (mocked)", {
    skip_on_cran()  # In case you don't want to run actual network ops on CRAN

    # Mock OS info (example: macOS on x86_64)
    mockSysInfo <- list(sysname = "Darwin", machine = "x86_64")
    mockDestDir <- tempdir()

    # We can temporarily override Sys.info() by using local mocking:
    with_mock(
        `Sys.info` = function() mockSysInfo,
        {
            # Instead of real downloading, mock out download.file
            mockDownloadFile <- function(url, destfile, mode, quiet) {
                # We can check that the URL is as expected:
                expect_true(grepl("darwin_x86_64", url))
                # "Fake" create a file to simulate a download
                file.create(destfile)
            }

            mockUnzip <- function(zipfile, exdir, ...) {
                # Create a directory as if we unzipped something
                dir.create(file.path(exdir, "agc-3.2"))
            }

            # Tar might be used if it’s a .tar.gz; let's just simulate the outcome
            mockUntar <- function(tarfile, exdir, ...) {
                dir.create(file.path(exdir, "agc-3.2"))
            }

            # Now run the test with the mocks
            with_mock(
                `utils::download.file` = mockDownloadFile,
                `utils::unzip`         = mockUnzip,
                `utils::untar`         = mockUntar,
                {
                    # Make a subdirectory to be sure it’s recognized
                    testDir <- file.path(mockDestDir, "agcTest")
                    dir.create(testDir, showWarnings = FALSE)

                    # Actually call the function
                    resultPath <- downloadAgcBinary(
                        destDir   = testDir,
                        version   = "3.2.1",  # e.g., "v3.2.1" on GitHub
                        verbose   = FALSE
                    )

                    # Check that the returned path is correct
                    expect_true(dir.exists(resultPath))
                    expect_true(basename(resultPath) == "agc_bin")

                    # The old directory "agc-3.2" should have been renamed to "agc_bin"
                    expect_false(dir.exists(file.path(testDir, "agc-3.2")))
                }
            )
        }
    )
})

test_that("downloadAgcBinary fails on unsupported OS/CPU", {
    skip_on_cran()

    # Mock OS info that doesn't exist in the fileMap
    mockSysInfo <- list(sysname = "UnknownOS", machine = "fancymachine")

    with_mock(
        `Sys.info` = function() mockSysInfo,
        {
            expect_error(
                downloadAgcBinary(destDir = tempdir(), version = "3.2.1"),
                regexp = "Unsupported chip architecture"
            )
        }
    )
})


## ----
# AGC
test_that("makeAgc requires AGC path in options", {
    # Ensure the option is not set
    oldOpt <- getOption("phgv2_agc_path")
    on.exit(options(phgv2_agc_path = oldOpt), add = TRUE)
    options(phgv2_agc_path = NULL)

    # Should raise an error
    expect_error(makeAgc(c("foo.fa"), "test.agc"), "AGC binary path not found")
})

test_that("makeAgc fails on invalid FASTA files", {
    # Set a dummy AGC path
    oldOpt <- getOption("phgv2_agc_path")
    on.exit(options(phgv2_agc_path = oldOpt), add = TRUE)
    options(phgv2_agc_path = "/path/to/agc")

    # Non .fa or .fasta file
    expect_error(
        makeAgc(c("not_a_fasta.txt"), "test.agc"),
        "All input files must be"
    )
})

test_that("makeAgc calls system2 with expected arguments", {
    # Set a dummy AGC path
    oldOpt <- getOption("phgv2_agc_path")
    on.exit(options(phgv2_agc_path = oldOpt), add = TRUE)
    options(phgv2_agc_path = "/dummy/path/to/agc")

    # Prepare mock FASTA files and AGC ID
    fastaFiles <- c("sample1.fa", "sample2.fa")
    agcId <- "output.agc"

    # Mock system2 to capture arguments
    mockSystem2 <- function(command, args, ...) {
        # Check that the command is our dummy path
        expect_identical(command, normalizePath("/dummy/path/to/agc", mustWork = FALSE))

        # We expect: c("create", "sample1.fa sample2.fa", ">", "output.agc")
        expect_equal(args[1], "create")
        expect_equal(args[2], "sample1.fa sample2.fa")
        expect_equal(args[3], ">")
        expect_equal(args[4], agcId)

        # Simulate a successful run
        return(0)
    }

    with_mock(
        `system2` = mockSystem2,
        {
            # If everything’s correct, makeAgc should return 0
            exitStatus <- makeAgc(fastaFiles, agcId)
            expect_equal(exitStatus, 0)
        }
    )
})

