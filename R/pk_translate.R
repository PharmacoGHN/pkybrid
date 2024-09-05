#' pk_data_translate
#'
#' These function are here to translate dataset as an in put to mapbayr
#'
#' @param id vector of patient identity
#' @param time vector containing all time of drug intake and observation
#' @param evid event id (1 = administration, 0 = drug sample) non mandatory parameters, will be calculated based on amt and dv
#' @param amt amount of drug given
#' @param cmt number of compartment of observation. cmt is set to 1 by default (central compartment)
#' @param rate infusion rate, set to 0 if route is Per Os (PO)
#' @param dv dependent variable (concentration observed)
#' @param mdv missing dependent variable (0 observation record DV is not missing, 1 = dose record or other time varying record, but dv is missing)
#' @param cov list of covariate and their values
#' @param route correspond to the route of administration, is set to PO by default
#'
#' @author Romain Garreau
#' @export


pk_data_translate <- function(id = NULL, time, evid = NA, cmt = 1, amt, rate = NULL, dv, mdv, cov = NULL, route = "PO") {
  if (is.null(id)) {
    id <- rep_len(1, length(time))
    rlang::warn("ID value was not provided. All ID are set to 1.")
  }

  data <- data.frame(
    ID = id,
    time = time,
    evid = evid,
    cmt = rep(cmt, length(time)),
    amt = amt,
    rate = rep(0, length(time)),
    DV = dv,
    mdv = mdv
  )

  if (is.na(evid)) {
    data$evid <- ifelse(data$amt == 0, 0, 1)
  }

  if (route != "PO") {
    if (is.null(rate)) { rlang::abort(message = "No infusion rate given, despite the drug not being administered by PO route")}
    data["rate"] <- rate
  }

  if (!is.null(cov)) {
    if (is.data.frame(cov)) {
      data <- cbind(data, cov)
    }
  }


  return(as.data.frame(data))
}

#' get_observation_time
#'
#' calculate the relative observation time

get_observation_time <- function() {

}