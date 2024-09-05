#' get_model
#'
#' @description help select model already existing in the package model bank.
#'
#' @param drug
#' @param software
#' @param name should be the name of the model. The convention used during package development is "FirstAuthorName.PublicationYear".
#' Example : Erin F Barreto, AAC, 2023 would be rename "Baretto.2023". While this is not mandatory it is easier to stick to this method as
#' the function is searching for attributes
#'
#' @author Romain Garreau
#' @export
#'

get_model <- function(drug, software, name, route = c("IV", "PO", "SC", "IM")) {
  model <- Filter(function(x) attr(x, "software") == software & attr(x, "study.id") == name & attr(x, "route") == route, model_bank[[drug]])
  if (length(model == 0)) warning("There is no model available for this drug. Check the available model using ?pkybrid::get_model")
  return(model)
}


model_bank <- list(
  isavuconazole = list(
    Perez.2023 = isavuco_icu_rrt_mod1
  ),
  cefepim = list(),
  vancomycin = list(),
  daptomycin = list(),
  rifampicin = list(),
  voriconazol = list(),
  amikacin = list()
)

# I. Isavuconazonium model ----
## I.a - Model by Perez et al.  Fungi, 2023.
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9960864/


isavuco_icu_rrt_mod1 <-
  "
$PARAM @annotated
TVCL: 3.98 : Clearance
TVV :  850 : Central Volume

ETA1: 0 : Clearance
ETA2: 0 : Central Volume


$PARAM @annotated @covariates
RRT : 0 : Presence of ERRT (1) or Absence (0)

$OMEGA 0.254 0.384
$SIGMA
0.17 // proportional
0 // additive


$CMT @annotated
CENT  : Central comparment [ADM, OBS]

$TABLE
double DV = CENT/V * (1 + EPS(1));

$MAIN
double CL = TVCL * exp(0.45 * RRT) * exp(ETA1 + ETA(1)) ;
double V = TVV * exp(ETA2 + ETA(2)) ;
double Ke = CL / V;


$ODE
dxdt_CENT = - Ke * CENT ;

$CAPTURE DV CL
"
attr(isavuco_icu_rrt_mod1, "software") <- "mrgsolve"
attr(isavuco_icu_rrt_mod1, "drug") <- "isavuconazole"
attr(isavuco_icu_rrt_mod1, "study.id") <- "Perez.2023"
attr(isavuco_icu_rrt_mod1, "route") <- "IV"
