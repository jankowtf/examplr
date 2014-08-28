#' @title 
#' Get Binary Example Status
#'
#' @description 
#' Checks for each function/method if an non-empty example has been been 
#' specified or not.
#'  
#' @template general-remarks
#' 
#' @param path \strong{Signature argument}.
#' 		Object containing directory path information 
#'   	(directory that contains the example script files).  
#' @param pattern \code{\link{character}}. Dimension: 1.
#' 		Pattern to recoginize empty example files.
#' 		Refactoring argument, do not change.
#' @template threedot
#' @template context-and-namespace
#' @example inst/examples/getBinaryExampleStatus.r
#' @seealso
#' \code{
#'  \link[rapp.core.examples]{getBinaryExampleStatus-character-missing-missing-method},
#'  \link[rapp.core.examples]{getBinaryExampleStatus-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export
setGeneric(
  name = "getBinaryExampleStatus",
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
    standardGeneric("getBinaryExampleStatus")       
  }
)

#' @title
#' Get Binary Example Status (\code{character-missing-missing)}
#'
#' @template general-remarks
#' 
#' @inheritParams getBinaryExampleStatus
#' @param path \code{\link{character}}. Dimension: 1. 
#'   	Directory path (directory that contains 
#' 		the example cript files).
#' @param .ctx \code{\link{missing}}. 
#' @param .ns \code{\link{missing}}.   
#' @return See method: 
#' 		\code{\link[rapp.core.examples]{getBinaryExampleStatus-character-RappCoreExamplesS3-RappCoreExamplesS3-method}}.
#' @example inst/examples/getBinaryExampleStatus.R
#' @seealso \code{
#' 		\link[rapp.core.examples]{getBinaryExampleStatus},
#' 		\link[rapp.core.examples]{getBinaryExampleStatus-character-RappCoreExamplesS3-RappCoreExamplesS3-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getBinaryExampleStatus", 
  signature = signature(
    path = "character",
    .ctx = "missing",
    .ns = "missing"
  ), 
  definition = function(
    path,
    pattern,
    ...,
    .ctx,
    .ns
  ) {
    
  #   ## Tracing //
  #   if (length(as.numeric(getOption("rapp")$$trace$tracelevel))) {
  #       
  #   }        
  
  return(getBinaryExampleStatus(
    path = path,
    pattern = pattern,
#     ...,
    .ctx = structure(NA, class = "RappCoreExamplesS3"),
    .ns = structure(NA, class = "RappCoreExamplesS3")
  ))

  }
)

#' @title
#' Get Binary Example Status (\code{character-RappCoreExamplesS3-RappCoreExamplesS3)}
#'
#' @template general-remarks
#' 
#' @inheritParams getBinaryExampleStatus
#' @param path \code{\link{character}}. Dimension: 1. 
#'     Directory path (directory that contains 
#' 		the example cript files).
#' @param .ctx \code{\link{RappCoreExamplesS3}}. 
#' @param .ns \code{\link{RappCoreExamplesS3}}.   
#' @return \code{logical}. Logical vector whose length corresponds to the 
#'    number of files identified in \code{path}. For each example file, 
#'    \code{TRUE} means actual example code exists, \code{FALSE} means it does not.
#' @example inst/examples/getBinaryExampleStatus.R
#' @seealso \code{
#' 		\link[rapp.core.examples]{getBinaryExampleStatus},
#' 		\link[rapp.core.examples]{getBinaryExampleStatus-character-missing-missing-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getBinaryExampleStatus", 
  signature = signature(
    path = "character",
    .ctx = "RappCoreExamplesS3",
    .ns = "RappCoreExamplesS3"
  ), 
  definition = function(
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
  
  ## Pattern modification //
  pattern <- paste0("^", pattern, "$")
  
  ## Validate //
  if (!file.exists(path)) {
    signalCondition(
      condition = "DirectoryDoesNotExist", 
      msg = c(
        "Example file directory does not exist",
        "Path" = path
      ),
      type = "error"
    )
  }
  if (!file.info(path)[,"isdir"]) {
    signalCondition(
      condition = "NotADirectory", 
      msg = c(
        "Path to example file directory is not a directory",
        "Path" = path
      ),
      type = "error"
    )
  }
  
  ## Get files //
  files <- list.files(path, full.names = TRUE)
  idx 	<- which(!file.info(files)[,"isdir"])
  if (!length(idx)) {
    signalCondition(
      condition = "NoExampleFilesFound", 
      msg = c(
        "No files found in example file directory",
        "Path" = path
      ),
      type = "error"
    )
  }
  files <- files[idx]
  
  ## Get status //
  out <- sapply(files, function(ii) {
    cnt <- readLines(ii)
    !length(grep(pattern, cnt)) > 0        
  })
  out
    
  }
)
