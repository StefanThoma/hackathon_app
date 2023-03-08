
library(Hmisc)

library(shiny)
library(tidyverse)
library(googledrive)
library(shinycssloaders)
library(shinybusy)
library(admiral)
library(admiral.test)

# for the tasks
#library(admiraldev)
library(haven)
library(metacore)
library(metatools)
library(xportr)




############### Setup Google Connection
## This sets token in project.
# # designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
#
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
#
# # trigger auth on purpose --> store a token in the specified cache
# drive_auth()

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)



# Event Parameters
end_of_event <- as_datetime("2023-03-03 17:00:00", tz = "America/Los_Angeles")



# Get folder location

pattrn <- "admiral_hackathon_2023"

dir = drive_find(q = c("createdTime > '2023-01-01T12:00:00'"), pattern = pattrn, type='folder', n_max = 1)

options(shiny.sanitize.errors = TRUE)

# local folders:
key <- "key" # expected dfs
sdtm <- "sdtm"
adam <- "adam"




## tasks and expected files:


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

key_list <- list("ADADAS" = c("USUBJID", "PARAMCD", "AVISIT", "ADT" ),
                 "ADAE" = c("USUBJID", "AETERM", "ASTDT", "AESEQ"),
                 "ADLBC" = c("USUBJID", "PARAMCD", "AVISIT", "LBSEQ"),
                 "ADLBH" = c("USUBJID", "PARAMCD", "AVISIT", "LBSEQ"),
                 "ADLBHY" = c("USUBJID", "PARAMCD", "AVISIT"),
                 "ADSL" = c("USUBJID"),
                 "ADTTE" = c("USUBJID", "PARAMCD"),
                 "ADVS" = c("USUBJID", "PARAMCD", "AVISIT", "ATPT", "VSSEQ"))


outcomes <- paste(tasks, "xpt", sep = ".")
task_df <- tibble(tasks, outcomes)


depends_list <- list("ADADAS" = c("ADSL"),
                     "ADAE" = c("ADSL"),
                     "ADLBC" = c("ADSL"),
                     "ADLBH" = c("ADSL"),
                     "ADLBHY" = c("ADSL"),
                     "ADSL" = NULL,
                     "ADTTE" = c("ADSL", "ADAE"),
                     "ADVS" = c("ADSL"))

names(depends_list) == names(key_list)
# import upload function
source("write_gdrive.R")
source("run_script.R")
source("compare_dfs.R")
source("register_team.R")
source("html_helpers.R")
source("get_depends.R")









# Define UI for data upload app ----
ui <- fluidPage(

  # App title
  titlePanel("Admiral Hackathon Submissions"),



  tags$head(
    tags$meta(name="author", content="Stefan Thoma"),
    tags$meta(name="creation_date", content="31/01/2023"),
    tags$link(rel = "shortcut icon", href = "favicon.ico")

  ),

  shiny::htmlOutput("timeToEnd"),
  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Register Team",
                           shiny::textInput("team_name", "Team Name"),
                           shiny::selectInput("n_members", "Number of Members", choices = 1:5, selected = 1),
                           shiny::uiOutput("name_email"),

                           # upload button
                           shiny::actionButton("register", label = "Register Group")
                  ),
                  tabPanel("Upload File",

                           fileInput("file", "Upload R File",
                                     multiple = FALSE,
                                     accept = c(".R")),


                           # team name of the group
                           uiOutput("team_options"),
                           #               shiny::textInput("team", "Team Name"),

                           # select task
                           shiny::selectInput("task",label = "Task", choices = tasks),

                           # upload button
                           shiny::actionButton("analyze", label = "Analyze")


                  ))),




    # Output: tabs ----
    mainPanel(


      tabsetPanel(type = "tabs",
                  tabPanel("Front Page", htmlOutput("frontpage")),
                  tabPanel("Preview Code", shinycssloaders::withSpinner(verbatimTextOutput("script"))),
                  tabPanel("Preview Table", shiny::br("First rows of the table created via the uploaded script:"),
                           shinycssloaders::withSpinner(tableOutput("table"))),
                  tabPanel("Analysis",
                           shinycssloaders::withSpinner(textOutput("score")),
                           shiny::verbatimTextOutput("diffdf")) #, uncomment this to include leaderboard.
                  #tabPanel("Summary", shinycssloaders::withSpinner(shiny::tableOutput("summary")))
                  )



    )

  ),
  hr(),
  h5("App Author: Stefan Thoma & Thomas Neitmann")
)

#  server  ----
server <- function(input, output, session){


  timer <- reactive({
    invalidateLater(1000, session)



    remaining <- difftime(end_of_event, Sys.time(), units = "hours") %>%
      as.numeric() %>%
      hms::hms(hours = .)

    active <- remaining > 0

   #active <- FALSE

    list(eoe = end_of_event, remaining = remaining, active = active)
  })

  output$timeToEnd <- renderPrint({


    if(timer()$active){

      HTML("Remaining time for submissions (in h:m:s): ",
            timer()$remaining %>%
              sub("\\..*", "", .),
           '<p style="color: #5e9ca0;"><span style="color: #000000;">---------------------------------------------------</span></p>')

    } else {

      HTML('<p style="color: #5e9ca0; text-align: center;"><span style="color: #000000;">Thank you for participating in the Admiral Hackathon.</span><br /><span style="color: #000000;">Submissions are closed now.</span><br /><span style="color: #000000;">You can still upload your scripts for now, but they will not be registered.</span></p>
             <p style="color: #5e9ca0; text-align: center;"><span style="color: #000000;">---------------------------------------------------</span></p>')
    }



  })


  # handle registration
  registrationData <-
    reactive({
      N <- input$n_members
      NAME <- sapply(1:N, function(i){paste0("name",i)})
      EMAIL <- sapply(1:N, function(i){paste0("email",i)})
      names <- character(0)
      emails <- character(0)

      for(i in 1:N){
        names[i] <- input[[paste0(NAME[i])]]
        emails[i] <- input[[paste0(EMAIL[i])]]
      }

      dplyr::tibble(team_name = input$team_name,
                    n_members = N,
                    member_name = names,
                    member_email = emails)


    }) %>%
    bindCache(input$team_name, input$n_members, input$name1, input$email1) %>%
    bindEvent(input$register)

  registrationProcess <- reactive({
    req(registrationData())



    #register_team
    reg <- register_team(reg_dat = registrationData())

    reg
  })
  n_mem <- reactive({
    input$n_members
  })

  dataModal <- function(failed = TRUE) {
    if(failed){
      modalDialog(
        div(tags$b("Team registration was not successful.", style = "color: red;")),
        HTML(vectorBulletList(registrationProcess()$message)),

        footer = tagList(
          modalButton("OK")
        ))
    } else if(timer()$active){
        modalDialog({


          tagList(HTML("<h1>Registration</h1>",
                       "<p>Congratulations, your team --"),
                  tags$b(input$team_name),
                  HTML("-- has successfully registered for the Admiral Hackathon.&nbsp;</p>"))

        }, renderTable((registrationData())),
        footer = tagList(
          modalButton("OK")
        )
        )


      } else{

        modalDialog({


          tagList(HTML("<h1>Registration</h1>",
                       "<p>The Hackathon has ended. &nbsp;</p>",
                       "<p>Your team --"),
                  tags$b(input$team_name),
                  HTML("-- has been added to the team selection.&nbsp;</p>"))

        }, renderTable((registrationData())),
        footer = tagList(
          modalButton("OK")
        )
        )

      }
  }





  observeEvent(input$register, {

    show_modal_spinner(
      spin = "spring",
      color = "dodgerblue",
      text = "Please wait..."
    )

    req(registrationProcess())

    remove_modal_spinner()
    showModal(
      dataModal(!registrationProcess()$checks$successful)


    )
  })


  output$name_email <- shiny::renderUI({
    N = n_mem()
    NAME = sapply(1:N, function(i){paste0("name",i)})
    EMAIL = sapply(1:N, function(i){paste0("email",i)})

    output = tagList()

    firstsecondthird <- c("First", "Second", "Third", "Fourth", "Fifth")
    for(i in seq_along(1:N)){
      output[[i]] = tagList()
      output[[i]] <- fluidRow(shiny::h4(paste(firstsecondthird[i], " Member")),
                              column(6,
                                     textInput(NAME[i], "Name"), value = " "),
                              column(6,
                                     textInput(EMAIL[i], "Email"), value = " "))
    }

    output


  })




  # update team options:
  output$team_options <-
    renderUI({
      selectInput("team", "Choose team", dirTeams()$name)
    })

  dirTeams <-
    reactive({
      input$register
      dir %>% drive_ls(type = "folder")
    })

  # handle script
  inputData <-
    reactive({

      list(
        "filepath" = input$file$datapath,
        "team" = input$team,
        "task" = input$task,
        "team_dir" = dir %>% drive_ls(type = "folder") %>% filter(name == input$team)
      )


    }) %>%
    bindCache(input$team, input$task, input$file) %>%
    bindEvent(input$analyze)

  shiny::observeEvent(input$analyze, {

    req(inputData())
    #

    if(timer()$active){

      out <- tryCatch(
        expr = {
          g_upload(r_file = inputData()$filepath,
                   team = inputData()$team,
                   task = inputData()$task,
                   team_dir = inputData()$team_dir)},
        error = function(e) {
          message('Uploading to drive failed and caused the following error: ')
          safeError(e)
        },
        warning = function(w) {
          message('A Warning Occurred while uploading: ')
          w
        }
      )

      out$name

    }
  })

  output$frontpage <- shiny::renderText({ # use https://onlinehtmleditor.dev/ to write html
    HTML("
<h1>Welcome to the Admiral Hackathon Application&nbsp;</h1>

<p>You can register your team with up to five members using the <strong>Register Team </strong>tab on the left.<br />
Please enter at least one valid email adress to participate in the Hackathon Contest.</p>

<p>Once you have registered, you can upload your R script in the <strong>Upload File</strong>&nbsp;tab on the left.<br />
Make sure you choose the right task and press <strong>analyze </strong>to run the R script you uploaded on our server.<br />
For every task, we expect a particular data file to be saved to the folder <strong>adam</strong>.<br />
This data file is then loaded and compared to our key file.</p>
    ")

  })


  #  output$try <- renderText({
  #    #source("sample_adsl.R")

  #    input$team
  #
  #  })


  ### run script -------

  user_data <- shiny::reactive({




    req(inputData())


    expected_file <- task_df %>% filter(tasks == inputData()$task) %>%
      select(outcomes)

    get_depends(task = inputData()$task,
                depends_list = depends_list,
                task_df = task_df,
                from = key,
                to = adam)

    # if(inputData()$task!="ADSL"){
    #   file.copy(file.path(key, "adsl.xpt"), adam)
    # }

    try(detach("package:Hmisc"))

    tryCatch(expr = source(inputData()$filepath, local = new.env()),
             error = function(e) paste("while sourcing the script the following error appeared: ", safeError(stop(e))))




    # check if file is there:
    #expected_file <- "ADSL.xpt"
    #adam <- "adam"


    path.to.expected <- list.files(adam, full.names = TRUE)[list.files(adam) %>%
                                           tolower() %in%
                                           {expected_file %>% tolower()}]


    if(length(path.to.expected) < 1){
    stop(safeError(paste(expected_file, " not found. Perhaps you had not selected the correct task? Files found: ", list.files(adam))))
  }
    # this would upload the file to google
        # for(i in files){

    #   inputData()$team_dir %>% drive_upload(media = i, overwrite = TRUE)
    # }


    user_data <- tryCatch(haven::read_xpt(path.to.expected),
                          error = function(e) stop(safeError(e)))



    unlink(list.files(adam, full.names = TRUE))
    #dir %>% drive_upload(media = )

    user_data
  })

  output$script <- shiny::renderText({
    head(readr::read_file(inputData()$filepath))
  })


  key_data <- shiny::reactive({
    # this should return a list with the relevant adam datasets, both key and user created


    expected_file <- task_df %>%
      filter(tasks == inputData()$task) %>%
      select(outcomes) %>% tolower()


    # save data
    haven::read_xpt(file.path(key, expected_file))

  })

  compare_df <- shiny::eventReactive(eventExpr = input$analyze, {

    req(inputData())
    req(user_data())
    req(key_data())
    difference <- tryCatch(expr = {compare_dfs(df_user = user_data(),
                                               df_key = key_data(),
                                               keys = key_list[[inputData()$task]])},
                           error = function(e) stop(safeError(e)))

    # send to google:
    if(timer()$active){
      tryCatch(expr = {score_to_google(score = difference$score,
                                       team = inputData()$team,
                                       task = inputData()$task
      )},
      error = function(e) stop(safeError(e)))
    }
    # output
    difference

  })

  output$diffdf <- shiny::renderPrint({
    results <- tryCatch({compare_df()$diffdf_result},
                        error = function(e) stop(safeError(e)))



    results
  })



  output$score <- shiny::renderText({

    paste("Your Score was: ", compare_df()$score)

  })


  output$table <- shiny::renderTable({
    req(inputData())

    tryCatch(expr = user_data() %>% head(),
             error = function(e) stop(safeError(e)))

  })


  output$summary <-
    shiny::renderTable({


      req(input$team)
      task_info <- tryCatch(
        expr = {

          tempdir <- tempfile(fileext = ".csv")
          dir %>% drive_ls() %>% filter(name == "task_info.csv") %>% drive_download(path = tempdir)

          readr::read_csv(tempdir)
        },
        error = function(e){NULL}
      )

      if(!is.null(task_info)){

        task_info <- task_info %>% select(-email) %>%
          filter(team == input$team) %>%
          group_by(team, task) %>%
          summarize(
            score = max(score))

      }else task_info <- tibble()

      task_info
    })



}
# Run the app ----
shinyApp(ui, server)
