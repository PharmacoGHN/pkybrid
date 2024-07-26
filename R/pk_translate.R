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
    if (is.null(rate)) {
      rlang::abort(message = "No infusion rate given, despite the drug not being administered by PO route")
    }
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
#' calculate the relative observation time from the first administration
#'
#' @param date is the date of observation
#' @param time is the time of observation
#' @param merged False by default. If set to true only time need to be given as a POSIXct
#' @param unit_scale scale used to calculate the time difference. Hours is the default
#'

get_observation_time <- function(date = NULL, time, merged = FALSE, unit_scale = "hours") {
  if (!unit_scale %in% c("secs", "mins", "hours", "days", "weeks")) {
    stop("Invalid unit_scale. Choose from 'secs', 'mins', 'hours', 'days', or 'weeks'.")
  }

  if (merged) {
    if (!inherits(time, "POSIXct")) {
      stop("When 'merged' is TRUE, 'time' must be a POSIXct object.")
    }
    rearranged_date_vector <- time
  } else {
    if (length(date) != length(time)) {
      stop("date and time are not the same length")
    }
    rearranged_date_vector <- as.POSIXct(paste(date, time))
  }

  # Calculate the time differences
  time_diff <- c(0, difftime(rearranged_date_vector[-1], rearranged_date_vector[1], units = unit_scale))

  return(time_diff)
}