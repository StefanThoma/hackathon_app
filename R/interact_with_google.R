# connect to google ------------------------------------------------------------
drive_auth()
gs4_auth(token = drive_token())

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)
# find project drive
pattrn <- "mock_hackathon"
dir <- drive_find(pattern = pattrn, type = "folder", n_max = 1)



#' check if string is valid email
#'
#' @param string
#'
#' @return boolean: TRUE if valid email, else FALSE
#' @examples
#' is_valid_email("stefan.thoma@roche.com")
#' is_valid_email("stefan.thoma@roche")
is_valid_email <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        as.character(x),
        ignore.case = TRUE)
}


#' register a team
#' if team name is valid:
#' create folder and add team information to google sheet
#' if team name is invalid, return message on what should be changed
#'
#' @param reg_dat registration information: df including `team_name`, `n_members`, `member_name` & `member_email`
#'
#' @return list of list `checks` and list `message` and `process`
#'
#' @examples
#'
#'  team_name <- rep(sample(letters, 8) %>% paste(collapse = ""), 4)
#'  n_members <- rep(4, 4)
#'  member_name <- c("Stefan Thoma", "", "", "")
#'  member_email <- c("stefan.thoma@roche.com", "", "", "")
#'  reg_data <- data.frame(team_name,
#'  n_members,
#'  member_email,
#'  member_name)
#'  register_team(reg_data)
#'
#'  reg_data2 <- data.frame(team_name = "stefan",
#'  n_members = 1,
#'  member_email = "stefan.thoma@roche.com",
#'  member_name = "stefan thoma")
#'
#'  register_team(reg_data2)
#'
register_team <- function(reg_dat) {
  message <- character()
  team_info <- NULL
  # extract info

  team_name <- unique(reg_dat$team_name)
  n_members <- unique(reg_dat$n_members)
  member_name <- reg_dat$member_name
  member_email <- reg_dat$member_email

  # check if team name is clean
  clean <- suppressWarnings(sentimentr::profanity(team_name)$profanity == 0)


  # check if there is enough information
  enough_info <- any(is_valid_email(member_email))

  # check if teamname is long enough
  name_valid <- nchar(team_name) > 1

  # check if team name is already taken
  folder <- dir %>% drive_ls(type = "folder")
  free <- !team_name %in% folder$name

  # if false and false create folder using team name
  process <- NULL



  if (!name_valid) {
    message <- c(message, "This team name is not valid.")
  }

  if (!enough_info) {
    message <- c(message, "A valid email address (for the first team member) has to be entered.")
  }
  if (!clean) {
    message <- c(message, "Please choose another team name.")
  }
  if (!free) {
    message <- c(
      message, "This team name is already taken.",
      "Have you registered already?"
    )
  }


  if (clean & free & name_valid & enough_info) {
    # create folder
    process <- tryCatch(
      expr =
        dir %>% drive_mkdir(name = team_name, overwrite = FALSE),
      error = function(e) e
    )
    successful <- !"error" %in% class(process)

    # create information file on team in team folder
    if (successful) {
      team_info <- reg_dat %>%
        mutate(time_created = Sys.time())

      sheet <- gs4_find("event_info")
      if(nrow(read_sheet(sheet, range = "teams"))<1){
        sheet %>%
          write_sheet(data = team_info, sheet = "teams")
      } else{
        sheet %>%
          sheet_append(data = team_info,
                       sheet = "teams")
      }
    }
  } else {
    successful <- FALSE
  }
  # pass back what worked / didn't. Whether it was successful.
  # no need to return path to team folder

  return(list(info = team_info, checks = list(clean = clean, free = free, successful = successful), process = process, message = message))
}

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
#' source(R / google_auth.R)
#' team <- "BaselRoche"
#' task <- "adsl"
#' r_file <- "register_team.R"
#' ttt <- dir %>% drive_ls()
#' team_dir <- ttt %>% filter(name == team)
#' g_upload(team = team, task = task, r_file = r_file, team_dir = team_dir)
#'
g_upload <- function(team, task, r_file, team_dir) {
  # create filename
  filename <- paste(team, "-", task, ".R", sep = "")

  team_dir %>% drive_upload(
    media = r_file,
    name = filename,
    overwrite = TRUE,
    type = "txt"
  )
  return(team_dir %>% drive_ls())
}


#' send score to google
#' this function takes the score and sends it to a google file with all the scores
#'
#' @param score
#' @param task
#' @param team
#'
#' @return
#' @export
#'
#' @examples
#' score_to_google(score = 1, task = "ADSL", team = "nqamojwb")
score_to_google <- function(score, task, team) {
  # get gsheet
  sheet <- gs4_find() %>% filter(name == "event_info")
  worksheet <- "submissions"

  # get email info
  team_info <- sheet %>% read_sheet(range = "teams") %>%
    filter(team_name == team)
  # extract only one valid email address from team data
  oneValidEmail <- team_info %>%
    filter(is_valid_email(member_email)) %>%
    head(1) %>%
    pull(member_email)

  task_info <- tibble(
    score = score,
    task = task,
    team = team,
    email = oneValidEmail,
    time = Sys.time()
  )

  # upload score

  if(nrow(read_sheet(sheet, range = worksheet))<1){
    sheet %>%
      write_sheet(data = task_info, sheet = worksheet)
  } else{
    sheet %>%
      sheet_append(data = task_info,
                   sheet = worksheet)
  }


  return(list(submission = task_info))
}



#' get leaderboard
#'
#' @return df with all best submissions by team and task
#' @export
#'
#' @examples
#' get_leaderboard()
#'
#'
#'
#'
get_leaderboard <- function(){
  # get gsheet
  sheet <- gs4_find() %>% filter(name == "event_info")
  worksheet <- "submissions"

  all_submissions <- sheet %>%
    read_sheet(sheet = worksheet) %>%
    select(-email) %>%
      group_by(team, task) %>%
    dplyr::summarize(
      score = max(score)
    )

  return(all_submissions)

}
