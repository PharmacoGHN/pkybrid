#' create_model
#'
#' create model is the generic function that allow user to very easily create a new model file
#'
#' @param type prespescified type of model.
#' @param constant specify if the model should be parametrized using micro of macro constant
#' @param cov is a list containing parameters and covariates relationship and beta value
#' @param engine nonmem by default. as of version 1.0.0 only nonmem engine is supported
#'
#' @export



create_model <- function(type, constant = c("micro", "macro"), cov = NULL, engine = "parametric") {

  if (!type %in% c("oral_1cp", "oral_2cp", "iv_1cp", "iv_2cp", "scim_1cp", "scim_2cp")) {
    stop("Invalid model type. Choose from 'oral_1cp', 'oral_2cp', 'iv_1cp', 'iv_2cp', 'scim_1cp', 'scim_2cp'")
  }

  if (!engine %in% c("parametric", "NPAG")) {
    stop("Invalid engine. Chose either parametric or NPAG.")
  }

  if (engine == "NPAG") {
    stop("NPAG is not available in V1.0")
  }

  if (!constant %in% c("micro", "macro")) {
    stop("Invalid constant. Model parameter should be 'macro' (CL, V etc.) or 'micro' (ke, V, etc.).")
  }

  model_type <- get_model_type(type, constant)


  # Temporary placehord until this line can be implemented : model_file <- get_model(model_type, cov, engine)
  model_file <- "placeholder_model_file"

  return(model_file)
}

#' get_model_type
#'
#' generated the model name to call generic model
#'
#' @param type type of model. see create model
#' @param constant type of constant used. see create model
#'
#' @noRd

get_model_type <- function(type, constant, tlag = FALSE) {

  modeltype <- strsplit(type,  split = "_")
  absorption_model <- ifelse(modeltype[[1]][1] == "iv", "", "_ka")

  parameters <- dplyr::case_when(
    modeltype[[1]][2] == "1cp" & constant  == "macro" ~ "Cl.V",
    modeltype[[1]][2] == "1cp" & constant  == "micro" ~ "ke.V",
    modeltype[[1]][2] == "2cp" & constant  == "macro" ~ "Cl.V.Q.V2",
    modeltype[[1]][2] == "2cp" & constant  == "micro" ~ "ke.V.k12.k21",
    .default = NA
  )

  if (is.na(parameters)) {
    stop("Non existing model. Please see ?pkybrid::get_model_type for more information")
  }

  model_name <- paste0(type, absorption_model, "_", parameters)


  if (tlag) {
    if (modeltype[[1]][1] == "iv") {stop("Can't have a Tlag with an iv model")}
    model_name <- paste0(model_name, "_Tlag")
  }

  return(model_name)
}