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
#' @template threedot
#' @template context-and-namespace
#' @example inst/examples/ensureExampleFiles.R
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
#' 		\code{\link[examplr]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExampleFiles.R
#' @seealso \code{
#' 		\link[examplr]{ensureExampleFiles},
#' 		\link[examplr]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
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
    ...,
    .ctx,
    .ns
  ) {
    
  ensureExampleFiles(pkg = pkg) 
    
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
#'     \code{\link[examplr]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExampleFiles.R
#' @seealso \code{
#' 		\link[examplr]{ensureExampleFiles},
#' 		\link[examplr]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
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
    ...,
    .ctx,
    .ns
  ) {
    
  return(ensureExampleFiles(
    pkg = pkg,
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
#'   	\code{\link[examplr]{ensureExampleFiles-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExampleFiles.R
#' @seealso \code{
#' 		\link[examplr]{ensureExampleFiles}
#' }
#' @template author
#' @template references
#' @export
setMethod(f = "ensureExampleFiles", 
  signature = signature(
    pkg = "character",
    .ctx = "RappCoreExamplesS3",
    .ns = "RappCoreExamplesS3"
  ), 
  definition=function(
    pkg,
    ...,
    .ctx,
    .ns
  ) {
    
#      if (!is.package(pkg)) {
#          create_description(pkg)
  pkg <- devtools::as.package(pkg)
  
  pattern_exampletag <- ".*@example (?=\\w*)"
  pattern_roxygen <- "^(#' ?|##' ?)"
  example_template <- c("\\dontrun{", "", "## TODO: add example", "", "}")            
  
  path_r <- file.path(pkg$path, "R")
  path_examples <- "inst/examples"
  path_examples_ref <- file.path(path_examples, "refs")
  
  ## Ensure directories //
  dir.create(path_examples, showWarnings=FALSE, recursive=TRUE)
  dir.create(path_examples_ref, showWarnings=FALSE)
  
  files <- list.files(path_r, full.names=TRUE, pattern="\\.[rR]$")
  ii=files[[1]]
  out <- unlist(lapply(files, function(ii) {
    out <- TRUE
    cnt <- readLines(ii)
    roxycode <- grep(pattern_roxygen, cnt, value=TRUE)
    if (length(roxycode)) {
      examples <- unique(grep(pattern_exampletag, roxycode, value=TRUE, perl=TRUE))
      if (length(examples)) {
        examples <- file.path(path_examples, basename(examples))
        out <- sapply(examples, function(ii) {
          ii <- gsub("\\.R$", ".r", ii)
          out <- file.exists(ii)
          if (!out) {
            write(example_template, file=ii)
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

