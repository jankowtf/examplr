#' Get Existing Example
#'
#' @description 
#' Gets names of functions/methods for which examples already exist.
#'  
#' @template general-remarks
#' 
#' @param path \strong{Signature argument}.
#' 		Object containing directory path information (directory that contains 
#' 		the examples' script files).  
#' @param pattern See \code{pattern} of 
#' 		\code{\link[rapp.core.examples]{getBinaryExampleStatus}}
#' @param include_fileext \code{\link{logical}}. Dimension: 1.
#'   	\code{TRUE} means return value includes file extensions,
#' 		\code{FALSE} means file extensions are removed.
#' 		Only considered if \code{return_path = FALSE}. Otherwise it is automatically
#' 		set to \code{TRUE}.   
#' @param return_path \code{\link{logical}}. Dimension: 1.
#' 		\code{TRUE} means return value corresponds to paths of example files,
#' 		\code{FALSE} (default) means return value corresponds to 
#' 		function/method names.
#' @template threedot
#' @template context-and-namespace
#' @seealso \code{
#'    \link[rapp.core.examples]{getExistingExamples-character-missing-missing-method},
#'    \link[rapp.core.examples]{getExistingExamples-character-RappCoreExamplesS3-RappCoreExamplesS3-method},
#'    \link[rapp.core.examples]{getExampleBinaryStatus},
#' 		\link[rapp.core.examples]{getMissingExamples}
#' }
#' @template author
#' @template references
#' @export
setGeneric(
  name = "getExistingExamples",
  signature = c("path", ".ctx", ".ns"),
  def=function(
    path,
    pattern = "## TODO: add example",
    include_fileext = FALSE,
    return_path = FALSE,
    ...,
    .ctx,
    .ns
  ) {
    standardGeneric("getExistingExamples")       
  }
)

#' @title
#' Get Existing Example (\code{character-missing-missing})
#'
#' @template see-generic
#' 
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @inheritParams getExistingExamples
#' @param path \code{\link{character}}. Dimension: 1. 
#'   	Directory path (directory that contains 
#' 		the example script files).
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return See method: 
#' 		\code{\link[rapp.core.examples]{getExistingExamples-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/getExistingExamples.R
#' @seealso \code{
#' 		\link[rapp.core.examples]{getExistingExamples},
#' 		\link[rapp.core.examples]{getExistingExamples-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getExistingExamples", 
  signature = signature(
    path = "character",
    .ctx = "missing",
    .ns = "missing"
  ), 
  definition = function(
    path,
    pattern,
    include_fileext,
    return_path,
    ...,
    .ctx,
    .ns
  ) {
    
#   ## Tracing //
#   if (length(as.numeric(getOption("rapp")$trace$tracelevel))) {
#     
#   }        
  
  ## Dispatch //
  out <- getExistingExamples(
    path = path,
    pattern = pattern,
    include_fileext = include_fileext,
    return_path = return_path,
    ...,
    .ctx = structure(NA, class = "RappCoreExamplesS3"),
    .ns = structure(NA, class = "RappCoreExamplesS3")
  )
  
  ## Return //
  return(out)
    
  }
)

#' @title
#' Get Existing Example (\code{character-RappCoreExamplesS3-RappCoreExamplesS3})
#'
#' @template see-generic
#' 
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @inheritParams getExistingExamples
#' @param path \code{\link{character}}. Dimension: 1. 
#'     Directory path (directory that contains 
#' 		the example script files).
#' @param include_fileext \code{\link{logical}}. Dimension: 1.
#' 		\code{TRUE} means return value includes file extensions,
#' 		\code{FALSE} means file extensions are removed.
#' 		Only considered if \code{return_path = FALSE}. Otherwise it is automatically
#' 		set to \code{TRUE}.   
#' @param return_path \code{\link{logical}}. Dimension: 1.
#' 		\code{TRUE} means return value corresponds to paths of example files,
#' 		\code{FALSE} (default) means return value corresponds to 
#' 		function/method names.
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return TODO
#' @example inst/examples/getExistingExamples.R
#' @seealso \code{
#' 		\link[rapp.core.examples]{getExistingExamples},
#' 		\link[rapp.core.examples]{getExistingExamples-character-missing-missing-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getExistingExamples", 
  signature = signature(
    path = "character",
    .ctx = "RappCoreExamplesS3",
    .ns = "RappCoreExamplesS3"
  ), 
  definition = function(
    path,
    pattern,
    include_fileext,
    return_path,
    ...,
    .ctx,
    .ns
  ) {
    
#     ## Tracing //
#     if (length(as.numeric(getOption("rapp")$trace$tracelevel))) {
#       
#     }        
    
    ## Modifications //
    if (return_path) {
      include_fileext <- TRUE
    }
    
    ## Get status //
    status <- getBinaryExampleStatus(path=path, pattern=pattern)
    out <- status[status]
    if (!length(out)) {
      out <- character()
    } else {
      out <- names(out)
      if (!return_path) {
        out <- basename(out)
      }    
      if (!include_fileext) {
        out <- gsub("\\.[rR]", "", out)
      }
    }
    return(out)
    
  }
)

