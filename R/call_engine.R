#' call_engine
#'
#' this set of function is designed to allow user to have different type of model
#' It allow to easily swap between software and method.
#'
#' @noRd

call_engine <- function(engine = "parametric") {
  if (!engine %in% c("parametric", "NPAG")) {
    stop("Invalid engine. Must choose betwee, 'parametric' and 'NPAG' (non parametric) engine")
  }

  if (engine == "parametric") {
    engine <- list(model_path <- system.file("models/parametrics", package = "pkybrid"))
    # call write function
    # call read function
  }

  return(engine)
}

#' call_model
#'
#' call model available in inst/models
#' all model should have been generated manually or with the help of this package.
#' Supported extension are C++ and Rcpp file.
#'
#' @param model_name name of model
#' @param engine select either parametric or non parametric model
#'
#' @export

call_model <- function(model_name, engine = "parametric") {

  if (!engine %in% c("parametric", "NPAG")) {
    stop("Invalid engine. Must choose between, 'parametric' and 'NPAG' (non parametric) engine")
  }

  engine <- call_engine(engine)
  model <- paste0(engine$model_path, strsplit(model_name, split =".")[[1]][1])

  pkmod <- mrgsolve::mcode(model)

  return(pkmod)
}
