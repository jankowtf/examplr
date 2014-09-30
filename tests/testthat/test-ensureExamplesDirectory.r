context("ensureExamplesDirectory-1")
test_that("ensureExamplesDirectory", {

    expect_is(res <- ensureExamplesDirectory(), "logical")
    expect_true(all(res))
        
  }
)
