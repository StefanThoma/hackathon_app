#team <- "StefanThoma3"
#names <- "Stefan Thoma, Thomas Neitmann"
#onmyown <- FALSE


options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)


# helping function to check if email is valid
is_valid_email <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}
#library(googledrive)

# Get folder location


pattrn <- "admiral_hackathon_2023"

dir = drive_find(q = c("createdTime > '2023-01-01T12:00:00'"), pattern = pattrn, type='folder')

register_team <- function(reg_dat){
  message <- character()

  # extract info

#  reg_dat <- trytibble

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
  free <- ! team_name %in% folder$name

  # if false and false create folder using team name
  process <- NULL



  if(!name_valid){
    message <- c(message, "This team name is not valid.")
  }

  if(!enough_info){
    message <- c(message, "A valid email address (for the first team member) has to be entered.")
  }
  if(!clean){
    message <- c(message, "Please choose another team name.")
  }
  if(!free){
    message <- c(message, "This team name is already taken.",
                 "Have you registered already?")
  }


    if(clean & free & name_valid & enough_info){

    # create folder
    process <- tryCatch(expr =
                             dir %>% drive_mkdir(name = team_name, overwrite = FALSE),
                           error = function(e) e)

    successful <- ! "error" %in% class(process)

    # create information file on team in team folder
    if(successful){
      team_info <- reg_dat %>%
        mutate(time_created = Sys.time())
      info_file <- tempfile(fileext = ".csv")
      write_csv(team_info, info_file)

      process %>% drive_upload(media = info_file,
                               type = "txt",
                               name = paste("teaminfo_", team_name, ".csv", sep = ""),
                               overwrite = FALSE)
    }
  } else successful <- FALSE


  # pass back what worked / didn't. Whether it was successful.
  # no need to return path to team folder
  return(list(checks = list(clean = clean, free = free, successful = successful), process = process, message = message))

}


#register_team(team, names, onmyown)




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
score_to_google <- function(score, task, team){
  # get email info
  tempdir1 <- tempfile(fileext = "csv")
  dir %>% drive_ls(type = "folder") %>% filter(name == team) %>%
    drive_ls() %>% filter(name == paste("teaminfo_", team, ".csv", sep = "")) %>% drive_download(path = tempdir1)
  team_info <- read_csv(tempdir1)

  oneValidEmail <- team_info %>% filter(is_valid_email(member_email)) %>% select(member_email)
  oneValidEmail <- oneValidEmail[[1]]
#score <- 9
#task <- "adsl"
#team <- "blue"

  #
  task_info <- tibble(score = score,
                      task = task,
                      team = team,
                      email = oneValidEmail,
                      time = Sys.time()
  )

  # download file:

  task_info_old <- tryCatch(
    expr = {

      tempdir <- tempfile(fileext = ".csv")
      dir %>% drive_ls() %>% filter(name == "task_info.csv") %>% head(1) %>% drive_download(path = tempdir)

      readr::read_csv(tempdir)
      },
    error = function(e){NULL}

    )
  if(!is_null(task_info_old)){

    task_info <- rbind(task_info_old, task_info)

  }

  task_file <- tempfile(fileext = ".csv")
  write_csv(task_info, task_file)

  dir %>% drive_upload(media = task_file,
                       type = "txt",
                       name = "task_info.csv",
                       overwrite = TRUE)
  return("OK")
}

