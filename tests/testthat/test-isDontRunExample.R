require("testthat")
test_that(desc="test_isDontRunExample",
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
    
    expect_error(isDontRunExample(path="nonexistingpath"))
    expect_equivalent(res <- isDontRunExample(path=path),
      c(TRUE, FALSE, FALSE, FALSE)
    )
    
  }
)
