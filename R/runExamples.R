#' @title
#' Run Examples
#'
#' @description
#' Run examples in \code{inst/examples}.
#'  
#' @details 
#' Note that you should apply this method \emph{after} a fresh initialization
#' of your \package{rapp} project as this will ensure that dummy example files
#' are created for all of your functions/methods.
#' 
#' @template general-remarks
#' 
#' @param path \strong{Signature argument}.
#'   	Object containing directory path information 
#'   	(directory that contains the example script files).   
#' @param dontrun \code{\link{logical}}. Dimension: 1.
#' 		\code{TRUE} means dont-run examples are included, 
#' 		\code{FALSE} (default) means only non-dont-run examples are considered.
#' @template threedot
#' @template context-and-namespace
#' @example inst/examples/runExamples.R
#' @template author
#' @template references
#' @export
setGeneric(
  name = "runExamples",
  signature = c(
    "path", 
    ".ctx", 
    ".ns"
  ),
  def = function(
    path,
    dontrun = FALSE,
    ...,
    .ctx,
    .ns
  ) {
    standardGeneric("runExamples")       
  }
)

#' @title
#' Run Examples (\code{character-missing-missing})
#' 
#' @template general-remarks
#'
#' @inheritParams runExamples 
#' @param path \code{character}. Dimension: 1. 
#'    Directory path.
#' @param .ctx \code{\link{missing}}. 
#'    Default context.
#' @param .ns \code{\link{missing}}. 
#'    Default namesapce.
#' @return See method:
#'   	\code{\link[rapp.core.examples]{runExamples-character-RappCoreExamplesS3-RappCoreExamplesS3}}
#' @example inst/examples/runExamples.R
#' @template author
#' @template references
#' @export
setMethod(
  f = "runExamples", 
  signature=signature(
    path = "character",
    .ctx = "missing",
    .ns = "missing"
  ), 
  definition = cmpfun(function(
    path,
    dontrun,
    ...,
    .ctx,
    .ns
  ) {
    
  return(
    runExamples(
      path = path,
      dontrun = dontrun,
      ...,
      .ctx = structure(NA, class = "RappCoreExamplesS3"),
      .ns = structure(NA, class = "RappCoreExamplesS3")
    )
  )

  }, options=list(suppressAll=TRUE))
)

#' @title
#' Run Examples (\code{character-RappCoreExamplesS3-RappCoreExamplesS3})
#' 
#' @template general-remarks
#'
#' @inheritParams runExamples 
#' @param path \code{character}. Dimension: 1. 
#'    Directory path.
#' @param .ctx \code{\link{RappCoreExamplesS3}}. 
#' @param .ns \code{\link{RappCoreExamplesS3}}. 
#' @return See method:
#' 		\code{\link[rapp.core.examples]{runExamples-character-RappCoreExamplesS3-RappCoreExamplesS3}}
#' @example inst/examples/runExamples.R
#' @template author
#' @template references
#' @export
setMethod(
  f = "runExamples", 
  signature=signature(
    path = "character",
    .ctx = "RappCoreExamplesS3",
    .ns = "RappCoreExamplesS3"
  ), 
  definition = cmpfun(function(
    path,
    dontrun,
    ...,
    .ctx,
    .ns
  ) {
    
#   ## Tracing //
#   if (length(as.numeric(getOption("rapp")$trace))) {
#     
#   }        
  
  ## Dont-Run pattern //
  pattern.dont <- "^\\\\dontrun{$"
  
  ## Get files //
  files <- getExistingExamples(path = path, return_path = TRUE)
  
  if (!length(files)) {
    out <- logical()
  } else {
    if (!dontrun) {
      idx 	<- isDontRunExample(path = path)
      files <- files[!idx]
    }
    if (!length(files)) {
      out <- logical()
    } else {
      ## Run //
      out <- sapply(files, function(ii) {
        cnt <- readLines(ii)
        idx <- grep(pattern.dont, cnt, perl = TRUE)
        if (length(idx)) {
          cnt <- cnt[c(-idx, -length(cnt))]
        }
        cnt <- paste(cnt, collapse="")
        expr <- parse(text=cnt)
        out <- tryCatch(
          {
            eval(expr)
            TRUE
          },
          warning=function(cond) {
            warning(conditionMessage(cond))
            invokeRestart("muffleWarning")
          },
          error=function(cond) {
            message(conditionMessage(cond)) 
            FALSE
          }
        )
      })
    }
  }
  return(out)

  }, options=list(suppressAll=TRUE))
)
