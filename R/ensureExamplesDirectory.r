#' @title
#' Ensure Examples Directory Structure
#'
#' @description 
#' Ensures examples directory (\code{/inst/examples}.
#' 
#' @template general-remarks
#' 
#' @param path \strong{Signature argument}.
#'    Object containing path information.
#' @template threedot
#' @template context-and-namespace
#' @example inst/examples/ensureExamplesDirectory.r
#' @seealso \code{
#'    \link[rapp2]{ensureExamplesDirectory-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "ensureExamplesDirectory", 
  signature = c(
    "path", 
    ".ctx",
    ".ns"
  ),
  def = function(
    path = "inst/examples",
    ...,
    .ctx,
    .ns
  ) {
    standardGeneric("ensureExamplesDirectory")
  }
)

#' @title
#' Ensure Examples Directory Structure (\code{missing-missing-missing})
#'
#' @template see-generic
#' 
#' @inheritParams ensureExamplesDirectory
#' @param path \code{\link{missing}}
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return See method: 
#' 		\code{\link[examplr]{ensureExamplesDirectory-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExamplesDirectory.r
#' @seealso \code{
#' 		\link[examplr]{ensureExamplesDirectory},
#' 		\link[examplr]{ensureExamplesDirectory-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(f = "ensureExamplesDirectory", 
  signature = signature(
    path = "missing",
    .ctx = "missing",
    .ns = "missing"
  ), 
  definition=function(
    path,
    ...,
    .ctx,
    .ns
  ) {
    
  ensureExamplesDirectory(path = path, ...) 
    
  }
)

#' @title
#' Ensure Examples Directory Structure (\code{character-missing-missing})
#'
#' @template see-generic
#' 
#' @inheritParams ensureExamplesDirectory
#' @param path \code{\link{character}}
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return See method: 
#'     \code{\link[examplr]{ensureExamplesDirectory-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExamplesDirectory.r
#' @seealso \code{
#' 		\link[examplr]{ensureExamplesDirectory},
#' 		\link[examplr]{ensureExamplesDirectory-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(f = "ensureExamplesDirectory", 
  signature = signature(
    path = "character",
    .ctx = "missing",
    .ns = "missing"
  ), 
  definition=function(
    path,
    ...,
    .ctx,
    .ns
  ) {
    
  return(ensureExamplesDirectory(
    path = path,
    ...,
    .ctx = structure(NA, class = "RappCoreExamplesS3"),
    .ns = structure(NA, class = "RappCoreExamplesS3")
  ))    
    
  }
)

#' @title
#' Ensure Examples Directory Structure (\code{character-RappCoreExamplesS3-RappCoreExamplesS3})
#'
#' @template see-generic
#' 
#' @inheritParams ensureExamplesDirectory
#' @param path \code{\link{character}}
#' @param .ctx \code{\link{RappCoreExamplesS3}}. 
#' @param .ns \code{\link{RappCoreExamplesS3}}.   
#' @return See method: 
#'   	\code{\link[examplr]{ensureExamplesDirectory-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/ensureExamplesDirectory.r
#' @seealso \code{
#' 		\link[examplr]{ensureExamplesDirectory}
#' }
#' @template author
#' @template references
#' @export
setMethod(f = "ensureExamplesDirectory", 
  signature = signature(
    path = "character",
    .ctx = "RappCoreExamplesS3",
    .ns = "RappCoreExamplesS3"
  ), 
  definition=function(
    path,
    ...,
    .ctx,
    .ns
  ) {
    
  out <- sapply(path, dir.create, recursive = TRUE, showWarnings = FALSE)
  out[1:length(out)] <- TRUE
    
  }
)

