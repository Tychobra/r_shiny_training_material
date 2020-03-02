
# setwd
rstudioapi

# pacakges
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyr,
  purrr,
  lubridate,
  readr,
  stringr,
  readxl,
  openxlsx,
  writexl,
  fs,
  fst,
  tibble,
  janitor,
  kableExtra,
  knitr,
  formattable
)

# data
from <- fs::path("H:\\ATLRFI\\Jimmy\\Data-Archive\\example-lossruns")
to <- fs::path("data")
fs::dir_copy(from, to, overwrite = FALSE)
