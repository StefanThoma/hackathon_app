# set parameters ---------------------------------------------------------------
# app authors
author <- c("Stefan Thoma", "Thomas Neitmann")

# Event Parameters
#end_of_event <- as_datetime("2023-03-03 17:00:00", tz = "America/Los_Angeles")
end_of_event <- lubridate::today()+1
# folder location



# local folders:
key <- "key" # expected dfs
sdtm <- "sdtm"
adam <- "adam"

## tasks and expected files:
# these are the tasks participants can chose to solve.
# for each task, we expect a particular adam dataset to be saved to the adam folder.
tasks <- c(
  "ADADAS",
  "ADAE",
  "ADLBC",
  "ADLBH",
  "ADLBHY",
  "ADSL",
  "ADTTE",
  "ADVS"
)
# These are the key variables specific to each adam dataset
key_list <- list(
  "ADADAS" = c("USUBJID", "PARAMCD", "AVISIT", "ADT"),
  "ADAE" = c("USUBJID", "AETERM", "ASTDT", "AESEQ"),
  "ADLBC" = c("USUBJID", "PARAMCD", "AVISIT", "LBSEQ"),
  "ADLBH" = c("USUBJID", "PARAMCD", "AVISIT", "LBSEQ"),
  "ADLBHY" = c("USUBJID", "PARAMCD", "AVISIT"),
  "ADSL" = c("USUBJID"),
  "ADTTE" = c("USUBJID", "PARAMCD"),
  "ADVS" = c("USUBJID", "PARAMCD", "AVISIT", "ATPT", "VSSEQ")
)
# outcomes defines the filenames we look for, for each task
outcomes <- paste(tasks, "xpt", sep = ".")
# combine tasks and outcomes to a df for easier indexing
task_df <- tibble(tasks, outcomes)
# for some tasks, participants are expected to use an already existing dataset
# this list maps the tasks to the datasets that are required to solve task
depends_list <- list(
  "ADADAS" = c("ADSL"),
  "ADAE" = c("ADSL"),
  "ADLBC" = c("ADSL"),
  "ADLBH" = c("ADSL"),
  "ADLBHY" = c("ADSL"),
  "ADSL" = NULL,
  "ADTTE" = c("ADSL", "ADAE"),
  "ADVS" = c("ADSL")
)

# testthat::expect_equal(names(depends_list), names(key_list)) %>% testthat::test_that(desc = "check if names match")
