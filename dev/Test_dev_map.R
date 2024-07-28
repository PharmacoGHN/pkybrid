library(mrgsolve)
library(mapbayr)


# load dataset
my_data <- data.frame(
  ID = 1, time = c(0, 1, 24, 24.5, 47.5, 48),
  evid = c(1, 0, 0, 1, 0, 0),
  cmt = 1,
  amt = c(850, 0, 0, 700, 0, 0),
  rate = c(1700, 0, 0, 1400, 0, 0),
  DV = c(NA, 87, 18, NA, NA, 20),
  mdv = c(1, 0, 0, 1, 1, 1),
  CRCL = 80,
  TBW = 90,
  SEX = 0,
  RIF = 0,
  AGE = 75
)
# load model
model <- mread("inst/models/parametrics/daptomycin")

# map estimate
my_est <- mapbayest(model, my_data) 
plot(my_est) |> plotly::ggplotly()
hist(my_est)

get_param(my_est, "CL", "V1")
