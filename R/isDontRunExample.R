#' @title 
#' Check for Don't-Run Examples
#'
#' @description 
#' Checks for each example files if the respective example is a "don't-run" 
#' example or not.
#' 
#' @template general-remarks
#' 
#' @param path \strong{Signature argument}.
#' 		Object containing directory path information (directory that contains 
#' 		the examples' script files).      
#' @param pattern See \code{pattern} of 
#' 		\code{\link[rapp.core.examples]{getBinaryExampleStatus}}.
#' @template threedot
#' @template context-and-namespace
#' @example inst/examples/isDontRunExample.R
#' @seealso \code{
#'    \link[rapp.core.examples]{isDontRunExample-character-missing-missing-method},
#'    \link[rapp.core.examples]{isDontRunExample-character-RappCoreExamplesS3-RappCoreExamplesS3-method},
#' }
#' @template author
#' @template references
#' @export
setGeneric(
  name = "isDontRunExample",
  signature = c(
    "path", 
    ".ctx", 
    ".ns"
  ),
  def = function(
    path,
    pattern = "## TODO: add example",
    ...,
    .ctx,
    .ns
  ) {
    standardGeneric("isDontRunExample")       
  }
)

#' @title 
#' Check for Don't-Run Examples (\code{character-missing-missing})
#'
#' @template see-generic
#' 
#' @inheritParams isDontRunExample
#' @param path \code{\link{character}}. Dimension: 1. 
#'   	Directory path (directory that contains 
#' 		the examples' script files).
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return See method: 
#' 		\code{\link[rapp.core.examples]{isDontRunExample-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/isDontRunExample.R
#' @seealso \code{
#' 		\link[rapp.core.examples]{isDontRunExample},
#' 		\link[rapp.core.examples]{isDontRunExample-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "isDontRunExample", 
  signature=signature(
    path = "character",
    .ctx = "missing",
    .ns = "missing"
  ), 
  definition = cmpfun(function(
    path,
    pattern,
    ...,
    .ctx,
    .ns
  ) {
    
#   ## Tracing //
#   if (length(as.numeric(getOption("rapp")$trace$tracelevel))) {
#     
#   }        
  
  ## Dispatch //
  return(isDontRunExample(
    path = path,
    pattern = pattern,
    ...,
    .ctx = structure(NA, class = "RappCoreExamplesS3"),
    .ns = structure(NA, class = "RappCoreExamplesS3")
  ))
    
  }, options=list(suppressAll=TRUE))
)

#' @title 
#' Check for Don't-Run Examples (\code{character-RappCoreExamplesS3-RappCoreExamplesS3})
#'  
#' @template see-generic
#' 
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @inheritParams isDontRunExample
#' @param path \code{\link{character}}. Dimension: 1. 
#'   	Directory path (directory that contains 
#' 		the examples' script files).
#' @param .ctx \code{\link{RappCoreExamplesS3}}. 
#' @param .ns \code{\link{RappCoreExamplesS3}}.  
#' @return \code{\link{logical}}. \code{TRUE} means example is a 
#' 		"dont-run" example, \code{FALSE} means it is not.
#' @example inst/examples/isDontRunExample.R
#' @seealso \code{
#' 		\link[rapp.core.examples]{isDontRunExample},
#' 		\link[rapp.core.examples]{isDontRunExample-character-missing-missing-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "isDontRunExample", 
  signature = signature(
    path = "character",
    .ctx = "RappCoreExamplesS3",
    .ns = "RappCoreExamplesS3"
  ), 
  definition = cmpfun(function(
    path,
    pattern,
    ...,
    .ctx,
    .ns
  ) {
    
#   ## Tracing //
#   if (length(as.numeric(getOption("rapp")$trace$tracelevel))) {
#     
#   }        
  
  ## Dont-Run pattern //
  pattern.dont <- "^\\\\dontrun{$"
  
  ## Get files //
  files <- getExistingExamples(path = path, pattern = pattern,
    return_path = TRUE)
  if (!length(files)) {
    out <- logical()
  } else {
    ## Get status //
    out <- sapply(files, function(ii) {
      cnt <- readLines(ii)
      length(grep(pattern.dont, cnt, perl = TRUE)) > 0        
    })
  }
  return(out)
    
  }, options=list(suppressAll=TRUE))
)

