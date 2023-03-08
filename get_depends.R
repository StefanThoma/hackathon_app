#' get_depends
#'
#' This function will add the files to the adam folder that are required for
#' any selectable task.
#'
#'
#' @param task current task
#' @param depends_list maps task to required filetypes
#' @param task_df maps filetypes to filenames
#' @param from where can the files be found
#' @param to to which folder should they be saved
#'
#' @return returns NULL either way, plus a message.
#' @export
#'
#' @examples
get_depends <- function(task, depends_list, task_df, from = "key", to = "adam"){

  file_temp <- depends_list[[task]]

  if(is.null(file_temp)){
    message("no adam datasets are required for this task.")
    return()
  }

  files_to_copy <- task_df %>%
    filter(task_df$tasks %in% file_temp) %>%
    select(outcomes) %>% as_vector() %>% tolower()


  path_from <- file.path(from, files_to_copy)

  file.copy(path_from, to)

  message(paste(task, " depends on ", paste(file_temp, collapse = " & "), ". The(se) file(s) was/were added to the ", to, " folder.", sep = ""))
  return()

}
