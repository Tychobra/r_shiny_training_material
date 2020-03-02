#  ------------------------------------------------------------------------
#
# Title : Importing Messy Excel Data, Pivot Tables, and Reshaping Data Tutorial
#    By : Jimmy Briggs
#  Date : 2019-09-10
#
#  ------------------------------------------------------------------------


# Recommended Prelimninary Reading ----------------------------------------

# R for Data Science Book - https://r4ds.had.co.nz/

# library packages --------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(readr)
library(stringr)
library(readxl)
library(openxlsx)
library(writexl)
library(fs)
library(fst)
library(tibble)
library(janitor)
library(knitr)
library(kableExtra)
library(formattable)
library(rstudioapi)

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# NOTE if you are missing any of these package run the following code:
# best practices would be to create a separate "dependencies.R" script which
# houses this code so the user can automatcially download all dependencies
# automatically if necessary i.e. source("dependencies.R)

source("dependencies.R")

# if (!require(pacman)) install.packages("pacman")
# pacman::p_load(
#   dplyr,
#   tidyr,
#   purrr,
#   lubridate,
#   readr,
#   stringr,
#   readxl,
#   openxlsx,
#   writexl,
#   fs,
#   fst,
#   tibble,
#   janitor,
#   kableExtra,
#   knitr,
#   formattable
# )

# data
# from <- fs::path("H:\\ATLRFI\\Jimmy\\Data-Archive\\example-lossruns")
# to <- fs::path("data")
# fs::dir_copy(from, to, overwrite = FALSE)

# load an example dataset/lossrun -----------------------------------------

# I saved some "demo" lossruns in the "data/lossruns" folder by evaluation date
# the "fs" pacakge is good for viewing and manipulating files and folders,
# here i will use fs::dir_ls() to list the xlsx files in the data folder:
path <- fs::path("data/example-lossruns")
files <- fs::dir_ls(path, type = "file", regexp = "~$", invert = TRUE, fixed = TRUE)
# dont worry aobut the regex ~$ above, this is to avoid displaying internal computer
# temporary files when you have them open (~$excel-file.xlsx) and avoid permission
# errors.

(basename(files))

# lets read in a single dataset first:
file <- files[length(files)] # pulls last file name in the files vector
file

# lets read in the data via the read_excel() function from readxl;
# alternatively you can use the openxlsx::read.xlsx funtion (see ?read.xlsx)
# they both have their advantages and disadvantages:
data_sample <- readxl::read_excel(file, sheet = 1, na = c("", " ", "NA"))
# Notice WARNINGS! - this is because readxl::read_excel attempts to "guess"
# column classes for you - you may get around this using the col_classes argument
# see ?read_excel

# lets look at the names
names(data_sample)

# base R's `str` function (structure) is useful for quick overviews of objects
str(data_sample)

# a more comprehensive overview can be utilized via tibble::glimpse
glimpse(data_sample)

# NOTES:
# note that there are a few issues of concern with the importation of the data:
# 1- the data is all stored as text fields in excel and therefore read in as "character"
# strings instead of dates, numbers, etc.
# 2- column names are surround by tick marks `` - which is not R-friendly

# lets create a "clean" data - to clean column names I will use the helpful
# janitor::clean_names function:
data_sample_clean <- data_sample %>%
  janitor::clean_names()

names(data_sample_clean)
# much better names

# now lets convert columnn classes

# first we will convert numeric columns to numbers (including dates)

# speficy numeric columns via dplyr::select_if() and contains():
num_cols <- data_sample_clean %>%
  select(contains("paid"),
         contains("incurred"),
         contains("reserve")) %>%
  names()

num_cols

# convert using mutate_at with all_of() to numeric see ?tidyselect::select_helpers
data_sample_clean <- data_sample_clean %>%
  mutate_at(vars(all_of(num_cols)), as.numeric)

# for more tedious numeric conversions conserider using readr::parse_number instead
# of base R as.numeric (i.e. if text contains commas, $'s , etc)

# for dates use the helpful openxlsx::convertToDate function which
# takes an integer value (like in excel) and converts to a date class
# via an "origin" - in excel the origin is usually 1900-01-01 - you can
# use openxlsx::getDateOrigin to verify

(origin <- openxlsx::getDateOrigin(file))

# note surrounding you call with ()'s automatically sends the object to the
# console to view i.e. (origin <- x) is the same as origin <- x; x

# use dplyr::mutate to convert columns - mutate_at allows you to manipulate
# multiple columns at once (there is also mutate_if)
# specify variable names with vars() and contains() to identify date columns

data_sample_clean <- data_sample_clean %>%
  # first need to pull numeric values from strings
  mutate_at(vars(contains("_date")), as.numeric) %>%
  # now convert to dates
  mutate_at(vars(contains("_date")), openxlsx::convertToDate, origin = origin)

class(data_sample_clean$loss_date) # Date !

glimpse(data_sample_clean) # dbl = number

# now, lets combine all those steps into a single custom read function
read_data <- function(path, cols, ...) {

  # verify path exists
  stopifnot(file.exists(path))

  hold <- readxl::read_excel(path, ...) %>%
    # clean names
    janitor::clean_names()

  # extract date origin specific to file
  origin <- openxlsx::getDateOrigin(path)

  # convert classes and return cleansed data
  hold %>%
    mutate_at(vars(contains("paid"),
                   contains("incurred"),
                   contains("reserve")),
              as.numeric) %>%
    mutate_at(vars(contains("_date")), openxlsx::convertToDate, origin = origin)


}

# test out our function
test <- read_data(path = file, na = c("", " ", "NA"))

all_equal(test, data_sample_clean) # identical to before


# loop through each file and merge into single DF -------------------------

# use purrr::map_dfr to loop and merge files

# in case other files have different columns lets only read in the columns
# we have in the data sample already read in
cols <- c(names(test), "file")

data_merged <- purrr::map_dfr(
  files,
  read_data,
  na = c("", " ", "NA"),
  .id = "file"
) %>%
  # remove extra cols
  select_at(cols)

# this loops through each file in files and applies the read_data function
# the .id argument adds a column speficying the file where the data came from
# see ?purrr::map

glimpse(data_merged)
View(data_merged)


# group by occurrence -----------------------------------------------------
# notice theres an occurrence ID field
data_grouped <- data_merged %>%
  group_by(occurrence_number, file) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup()

# now we have the financials summed together by occurrence
# lets add them to the claims data via a left_join

data_w_occ <- data_merged %>%
  # mutate in a eval date col based off file name
  mutate(
    eval_date = basename(file) %>%
      stringr::str_replace(".xlsx", "")
    %>% lubridate::ymd()
  ) %>%
  # add in occurrence grouped values
  # the by tells the join which id columns to join by and the
  # suffic argument adds _clm to any claim fields and _occ to any occurrence fields
  left_join(data_grouped, by = c("occurrence_number", "file"), suffix = c("_clm", "_occ"))


# pivot table -------------------------------------------------------------

# lets create a pivot table which summarises numeric fields by eval and coverage

# additionally we will group by occurrence.

pivot_table_data <- data_w_occ %>%
  # group by eval and coverage
  group_by(eval_date, coverage) %>%
  # summarise financials
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup()

# now lets spread out total_incurred_by occurrence by coverage
pivot_table <- pivot_table_data %>%
  select(eval_date, coverage, incurred_total_occ) %>%
  # spread out total_inccurred by occurrence by coverage
  tidyr::pivot_wider(names_from = "coverage", values_from = "incurred_total_occ")

View(pivot_table)

# lets see it:
out <- pivot_table %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  mutate_if(is.numeric, as.character) %>%
  mutate_at(vars(-eval_date), formattable::currency, digits = 0)

knitr::kable(out,
             align = "c",
             format = "html",
             caption = "Total Incurred by Evaluation Date and Coverage (Grouped by Occurrence)") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "condensed", "responsive")
    )

# looks good

# can write to excel like this:
writexl::write_xlsx(out, "output.xlsx")
