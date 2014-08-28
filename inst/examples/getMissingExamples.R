\dontrun{

## Assuming you are calling from within a package project //
getMissingExamples(path = "inst/examples")

try(getMissingExamples(path = "non-existing-path"))

}
