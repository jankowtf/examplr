context("package")

test_that("ensureExampleFiles", {

    expect_is(res <- ensureExampleFiles(), "logical")
    expect_true(all(res))
        
  }
)
