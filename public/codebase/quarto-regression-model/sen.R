# https://bluefoxr.github.io/COINrDoc/sensitivity-analysis.html
# https://www.r-bloggers.com/2020/12/what-makes-a-sensitivity-analysis/
# https://complementarytraining.net/simple-sensitivity-analysis-with-r/

# devtools::install_github("bluefoxr/COINr6")
library(COINr6)
ASEM <- build_ASEM()

specs <- data.frame(AgLevel = c(2,3), NoiseFactor = c(0.25,0.25))

SAspecs <- list(
  impute = list(imtype = c("indgroup_mean", "ind_mean", "none")),
  normalise = list(ntype = c("minmax", "rank", "dist2max")),
  weights = list(NoiseSpecs = nspecs, Nominal = "Original")
)
