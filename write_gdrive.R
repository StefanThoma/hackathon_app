


#' Save script to google
#' this function will take the uploaded script and save it to a team specific google folder.
#'
#' @param team
#' @param task which of the tasks is the file supposed to solve?
#' @param r_file path to file containing code to solve the task
#' @param team_dir where to save it to in google. This is a tibble.
#'
#' @return paths to all files on the team folder.
#' @export
#'
#' @examples
g_upload <- function(team, task, r_file, team_dir){

  # create filename
  filename <- paste(team, "-", task, ".R", sep ="")

  team_dir %>% drive_upload(media = r_file,
                            name = filename,
                            overwrite = TRUE,
                            type = "txt")
  return(team_dir %>% drive_ls())
}



# TRY OUT


#team <- "BaselRoche"
#task <- "adsl"
#r_file <- "register_team.R"
#ttt <- dir %>%  drive_ls()
 #team_dir <- ttt %>%filter(name==team)


#

#done <- g_upload(team = team, task = task, r_file = r_file, team_dir = team_dir)


