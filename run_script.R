#content <- "adsl <- data.frame(1:3, 3:5)"
#  r_file <- tempfile()
#  writeLines(text = content, r_file)

run_script <- function(r_file = NULL, team, task = "adsl", team_dir = NULL){
  temp.file <- tempfile(fileext = ".R")
  readr::write_lines(r_file, file = temp.file)

  tryCatch(
    expr = source(readLines(temp.file)),
    error = function(e) {
      message('Running the script caused the following error: ')
      print(e)
    },
    warning = function(w) {
      message('A Warning Occurred')
      print(w)
    }
  )
  inFolder <- list.files(adam)

  expected <- switch(task,
                     "adsl" = "adsl.xpt",
                     "ae" = "ae.xpt")


  isItIn <- expected %in% inFolder

  if(isItIn){
    team_dir %>% drive_upload(media  = paste(adam, expected, sep = "/"))
  }


  return(isItIn)



}




