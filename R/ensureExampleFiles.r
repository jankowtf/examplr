#' @title
#' Ensure Example Files
#'
#' @description 
#' Ensures that example files specified via \code{@@example} exist.
#' 
#' @template general-remarks
#' 
#' @param pkg \strong{Signature argument}.
#' 		Package name or path to R package project.
#' @param strict \code{\link{logical}}.
#'    Should certain conditions trigger an error (\code{TRUE}) or only a 
#'    warning (\code{FALSE}, default)?
#'    Currently, the condition is met when the extension of an example file
#'    path does not comply with the convention of using \code{.r} as the 
#'    extension for R files (instead of \code{.R}).
#' @template threedot
#' @template context-and-namespace
#' @example inst/examples/ensureExampleFiles.r
#' @seealso \code{
#'    \link[rapp2]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "ensureExampleFiles", 
  signature = c(
    "pkg", 
    ".ctx",
    ".ns"
  ),
  def = function(
    pkg = ".",
    strict = FALSE,
    ...,
    .ctx,
    .ns
  ) {
    standardGeneric("ensureExampleFiles")
  }
)

#' @title
#' Ensure Example Files (\code{missing-missing-missing})
#'
#' @template see-generic
#' 
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @inheritParams ensureExampleFiles
#' @param pkg \code{\link{missing}}
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return See method: 
#' 		\code{\link[rapp.core.examples]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExampleFiles.r
#' @seealso \code{
#' 		\link[rapp.core.examples]{ensureExampleFiles},
#' 		\link[rapp.core.examples]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(f = "ensureExampleFiles", 
  signature = signature(
    pkg = "missing",
    .ctx = "missing",
    .ns = "missing"
  ), 
  definition=function(
    pkg = ".",
    strict,
    ...,
    .ctx,
    .ns
  ) {
    
  ensureExampleFiles(pkg = pkg, strict = strict, ...) 
    
  }
)

#' @title
#' Ensure Example Files (\code{character-missing-missing})
#'
#' @template see-generic
#' 
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @inheritParams ensureExampleFiles
#' @param pkg \code{\link{character}}
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return See method: 
#'     \code{\link[rapp.core.examples]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExampleFiles.r
#' @seealso \code{
#' 		\link[rapp.core.examples]{ensureExampleFiles},
#' 		\link[rapp.core.examples]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(f = "ensureExampleFiles", 
  signature = signature(
    pkg = "character",
    .ctx = "missing",
    .ns = "missing"
  ), 
  definition=function(
    pkg,
    strict,
    ...,
    .ctx,
    .ns
  ) {
    
  return(ensureExampleFiles(
    pkg = pkg,
    strict = strict,
    ...,
    .ctx = structure(NA, class = "RappCoreExamplesS3"),
    .ns = structure(NA, class = "RappCoreExamplesS3")
  ))    
    
  }
)

#' @title
#' Ensure Example Files (\code{character-RappCoreExamplesS3-RappCoreExamplesS3})
#'
#' @template see-generic
#' 
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @inheritParams ensureExampleFiles
#' @param pkg \code{\link{character}}
#' @param .ctx \code{\link{RappCoreExamplesS3}}. 
#' @param .ns \code{\link{RappCoreExamplesS3}}.   
#' @return See method: 
#'   	\code{\link[rapp.core.examples]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExampleFiles.r
#' @seealso \code{
#' 		\link[rapp.core.examples]{ensureExampleFiles}
#' }
#' @template author
#' @template references
#' @export
#' @import devtools
#' @import rapp.core.condition
setMethod(f = "ensureExampleFiles", 
  signature = signature(
    pkg = "character",
    .ctx = "RappCoreExamplesS3",
    .ns = "RappCoreExamplesS3"
  ), 
  definition=function(
    pkg,
    strict,
    ...,
    .ctx,
    .ns
  ) {
  
  ## Strictness //
  cond_type <- switch(strict, "TRUE" = "error", "FALSE" = "warning")    

#      if (!is.package(pkg)) {
#          create_description(pkg)
  pkg <- devtools::as.package(pkg)
  
  ## Patterns //
  pattern_exampletag <- ".*@example (?=\\w*)"
  pattern_roxygen <- "^(#' ?|##' ?)"
  
  ## Templates //
  example_template <- c("\\dontrun{", "", "## TODO: add example", "", "}")            
  
  ## Paths //
  path_r <- file.path(pkg$path, "R")
  path_examples <- "inst/examples"
  path_examples_ref <- file.path(path_examples, "refs")
  
  ## Ensure directories //
  dir.create(path_examples, showWarnings=FALSE, recursive=TRUE)
  dir.create(path_examples_ref, showWarnings=FALSE)
  
  files <- list.files(path_r, full.names=TRUE, pattern="\\.[rR]$")
#   ii=files[[2]]
  out <- unlist(lapply(files, function(ii) {
    out <- TRUE
    cnt <- readLines(ii)
    roxycode <- grep(pattern_roxygen, cnt, value=TRUE)
    if (length(roxycode)) {
#       examples <- unique(grep(pattern_exampletag, roxycode, value=TRUE, perl=TRUE))
      idx <- unique(grep(pattern_exampletag, roxycode, perl=TRUE))
      
      if (length(idx)) {
        examples <- unique(roxycode[idx])
        examples <- file.path(path_examples, basename(examples))
#         ii_2=examples[1]
        out <- sapply(examples, function(ii_2) {
          if (length(grep("\\.R$", ii_2))) {
            rapp.core.condition::signalCondition(
              condition = "ConventionViolation:FileExtension",
              msg = c(
                "Convention violation: file extension of example file path",
                File = ii,
                Path = ii_2,
                "Expected file extension" = ".r"
              ),
              ns = "rapp.core.examples",
              type = cond_type
            )
          }
          ii_2 <- gsub("\\.R$", ".r", ii_2)
          if (!length(grep("\\.r$", ii_2))) {
            rapp.core.condition::signalCondition(
              condition = "InvalidExampleFilePath",
              msg = c(
                "Invalid example file path",
                File = ii,
                Path = ii_2
              ),
              ns = "rapp.core.examples",
              type = "error"
            )
          }
          
          out <- file.exists(ii_2)
          if (!out) {
            write(example_template, file=ii_2)
            out <- TRUE
          } 
          out
        })
        out            
      }
    }
    out
  }))
  
  out 
    
  }
)

