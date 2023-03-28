## first we get data
# df_user <- haven::read_xpt("solution/adsl.xpt")
# df_key <- haven::read_xpt("key/adsl.xpt")
# df_user <- df_user %>%
#  mutate(TRTEDT = as.character(TRTEDT))
#
# df_user <- df_user %>%
#    rename(stdudyijd = STUDYID) %>%
#  mutate(TRTEDT = as.character(TRTEDT))
# install.packages("diffdf")
library(diffdf)
library(tidyverse)



#' Scoring function
#'
#' @param n number of warnings generated
#'
#' @return
#' @export
score_f <- function(df_user, df_key, keys) {
  score <- 10
  diff <- diffdf::diffdf(df_user, df_key, keys = keys)
  if (!diffdf::diffdf_has_issues(diff)) {
    return(score)
  }

  if (!diffdf::diffdf_has_issues(diffdf::diffdf(df_user,
    df_key,
    keys = keys,
    strict_numeric = FALSE,
    strict_factor = FALSE
  ))) {
    return(score - 1)
  }

  return(round(min(max(score - length(diff) / 3, 1), 9), 2))
}


#' Compare data-frames
#'
#' @param df_user
#' @param df_key
#'
#' @return the result of the diffdf function
#' @export
#'
#' @examples
compare_dfs <- function(df_user, df_key, keys) {
  result <- diffdf::diffdf(df_user, df_key, keys = keys)
  score <- score_f(df_user, df_key, keys)


  return(list("score" = score, "diffdf_result" = result))
}
