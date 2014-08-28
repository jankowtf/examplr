require("testthat")
test_that(desc="getMissingExamples",
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
    
    expect_error(res <- getMissingExamples(path = "nonexistingpath"))
    expect_equivalent(res <- getMissingExamples(path=path),
      "fooA"
    )
    expect_equivalent(
      res <- getMissingExamples(path=path, include_fileext=TRUE),
      "fooA.R"
    )
    expect_equivalent(
      res <- getMissingExamples(path=path, return_path=TRUE),
      c(file.path(path, "fooA.R"))
    )
    expect_true(length(grep("/", res)) > 0)
    
  }
)
