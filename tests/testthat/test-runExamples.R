context("runExamples-1")
test_that(desc="runExamples",
  code={
    
    ##--------------------------------------------------------------------------
    ## path: character
    ## .ctx: missing
    ## .ns: missing
    ##--------------------------------------------------------------------------
    
    if (basename(getwd()) == "testthat") {
      path <- "data"
    } else {
      path <- "tests/testthat/data"
    }
    
    expect_error(runExamples(path="nonexistingpath"))
    expect_equivalent(
      res <- runExamples(path = path),
      c(TRUE, TRUE, TRUE)
    )
    
    if (basename(getwd()) == "testthat") {
      path <- "../../inst/examples"
    } else {
      path <- "inst/examples"
    }
    
    expect_is(res <- runExamples(path = path), "logical")
    expect_is(res <- runExamples(path = path, dontrun = TRUE), "logical")
  }
)
