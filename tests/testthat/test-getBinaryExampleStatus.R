context("getBinaryExampleStatus-1")
test_that(desc="getBinaryExampleStatus",
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
    
    expect_error(res <- getBinaryExampleStatus(path = "nonexistingpath"))
    expect_equivalent(
      res <- getBinaryExampleStatus(path = path),
      c(FALSE, rep(TRUE, 4))
    )
    
  }
)
