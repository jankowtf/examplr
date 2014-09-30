#' @title
#' Get Missing Examples
#'
#' @description 
#' Gets names of functions/methods for which examples are still missing.
#'  
#' @template general-remarks
#' 
#' @param path \strong{Signature argument}.
#' 		Object containing directory path information (directory that contains 
#' 		the examples' script files).  
#' @param pattern See \code{\link[examplr]{getBinaryExampleStatus}}.
#' @param include_fileext See \code{\link[examplr]{getExistingExamples}}.   
#' @param return_path See \code{\link[examplr]{getExistingExamples}}.   
#' @template threedot
#' @template context-and-namespace
#' @example inst/examples/getMissingExamples.r
#' @seealso \code{
#'    \link[examplr]{getMissingExamples-character-missing-missing-method},
#'    \link[examplr]{getMissingExamples-character-RappCoreExamplesS3-RappCoreExamplesS3-method},
#' 		\link[examplr]{getExampleBinaryStatus},
#' 		\link[examplr]{getExistingExamples}
#' }
#' @template author
#' @template references
#' @export
setGeneric(
  name = "getMissingExamples",
  signature=c(
    "path", 
    ".ctx", 
    ".ns"
  ),
  def = function(
    path,
    pattern = "## TODO: add example",
    include_fileext = FALSE,
    return_path = FALSE,
    ...,
    .ctx,
    .ns
  ) {
    standardGeneric("getMissingExamples")       
  }
)

#' Get Existing Example
#'
#' @template see-generic
#' 
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @inheritParams getMissingExamples
#' @param path \code{\link{character}}. Dimension: 1. 
#'   	Directory path (directory that contains 
#' 		the examples' script files).
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return See method: 
#' 		\code{\link[examplr]{getMissingExamples-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/getMissingExamples.r
#' @seealso \code{
#' 		\link[examplr]{getMissingExamples},
#' 		\link[examplr]{getMissingExamples-character-RappCoreExamplesS3-RappCoreExamplesS3-method},
#'   	\link[examplr]{getExistingExamples},
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getMissingExamples", 
  signature=signature(
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
  return(getMissingExamples(
    path = path,
    pattern = pattern,
    include_fileext = include_fileext,
    return_path = return_path,
    ...,
    .ctx = structure(NA, class = "RappCoreExamplesS3"),
    .ns = structure(NA, class = "RappCoreExamplesS3")
  ))
    
  }
)

#' @title 
#' Get Missing Examples (\code{character-RappCoreExamplesS3-RappCoreExamplesS3})
#' 
#' @template see-generic
#' 
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @inheritParams getMissingExamples
#' @param path \code{\link{character}}. Dimension: 1. 
#'   	Directory path (directory that contains 
#' 		the examples' script files).
#' @param .ctx \code{\link{RappCoreExamplesS3}}. 
#' @param .ns \code{\link{RappCoreExamplesS3}}.  
#' @return \code{\link{character}}. Names of functions/methods for which 
#' 		there exist no examples yet.
#' @example inst/examples/getMissingExamples.r
#' @seealso \code{
#' 		\link[examplr]{getBinaryExampleStatus},
#' 		\link[examplr]{getMissingExamples-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @export
#' @template author
#' @template references
setMethod(
  f = "getMissingExamples", 
  signature=signature(
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
    
#   ## Tracing //
#   if (length(as.numeric(getOption("rapp")$trace$tracelevel))) {
#     
#   }        
  
  ## Modifications //
  if (return_path) {
    include_fileext <- TRUE
  }
  
  ## Get status //
  status <- getBinaryExampleStatus(path = path, pattern = pattern)
  out <- status[!status]
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
