require("testthat")
test_that(desc="test_getExistingExamples",
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
    
    expect_error(
      res <- getExistingExamples(path="nonexistingpath")
    )
    expect_equivalent(
      res <- getExistingExamples(path = path),
      c("fooB", "fooC", "fooD", "fooE")
    )
    expect_equal(
      res <- getExistingExamples(path=path, include_fileext=TRUE),
      c("fooB.R", "fooC.R", "fooD.R", "fooE.R")
    )
    expect_equivalent(
      res <- getExistingExamples(path=path, return_path=TRUE),
      c(
        file.path(path, "fooB.R"),
        file.path(path, "fooC.R"),
        file.path(path, "fooD.R"),
        file.path(path, "fooE.R")
      )
    )
    expect_true(length(grep("/", res)) > 0)
    
  }
)
