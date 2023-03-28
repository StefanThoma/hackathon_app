# packages ---------------------------------------------------------------------
#library(Hmisc)

library(shiny)
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(shinycssloaders)
library(shinybusy)
library(admiral)
library(admiral.test)

# for the tasks
# library(admiraldev)
library(haven)
library(metacore)
library(metatools)
library(xportr)

# make app errors appear locally as if deployed
options(shiny.sanitize.errors = TRUE)

# source scripts ---------------------------------------------------------------
# source("R/google_auth.R")
source("R/parameters.R")
source("R/compare_dfs.R")
source("R/interact_with_google.R")
source("R/html_helpers.R")
source("R/get_depends.R")

# app UI -----------------------------------------------------------------------
ui <- fluidPage(

  # App title
  titlePanel("Admiral Hackathon Submissions"),
  tags$head(
    tags$meta(name = "author", content = paste(author, collapse = ", ")),
    tags$meta(name = "creation_date", content = "31/01/2023")
  ),
  shiny::htmlOutput("timeToEnd"),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    ## inputs ------------------------------------------------------------------
    sidebarPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Register Team",
          shiny::textInput("team_name", "Team Name"),
          shiny::selectInput("n_members", "Number of Members", choices = 1:5, selected = 1),
          shiny::uiOutput("name_email"),

          # upload button
          shiny::actionButton("register", label = "Register Group")
        ),
        tabPanel(
          "Upload File",
          fileInput("file", "Upload R File",
            multiple = FALSE,
            accept = c(".R")
          ),


          # team name of the group
          uiOutput("team_options"),

          # select task
          shiny::selectInput("task", label = "Task", choices = tasks),

          # upload button
          shiny::actionButton("analyze", label = "Analyze")
        )
      )
    ),




    ## outputs -----------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Front Page", htmlOutput("frontpage")),
        tabPanel("Preview Code", shinycssloaders::withSpinner(verbatimTextOutput("script"))),
        tabPanel(
          "Preview Table", shiny::br("First rows of the table created via the uploaded script:"),
          shinycssloaders::withSpinner(tableOutput("table"))
        ),
        tabPanel(
          "Analysis",
          shinycssloaders::withSpinner(textOutput("score")),
          shiny::verbatimTextOutput("diffdf")
        ),
        # included this to add leaderboard.
        tabPanel("Summary", shinycssloaders::withSpinner(shiny::tableOutput("summary")))
      )
    )
  ),
  hr(),
  h5("App Author: ", paste(author, collapse = " & "))
)

server <- function(input, output, session) {
  ## countdown -----------------------------------------------------------------
  # create countdown to end of event
  timer <- reactive({
    invalidateLater(1000, session)
    remaining <- difftime(end_of_event, Sys.time(), units = "hours") %>%
      as.numeric() %>%
      hms::hms(hours = .)

    active <- remaining > 0

    list(eoe = end_of_event, remaining = remaining, active = active)
  })

  # render countdown to end of event & add message
  output$timeToEnd <- renderPrint({
    if (timer()$active) { # during hackathon display:
      HTML(
        "Remaining time for submissions (in h:m:s): ",
        timer()$remaining %>%
          sub("\\..*", "", .),
        '<p style="color: #5e9ca0;"><span style="color: #000000;">---------------------------------------------------</span></p>'
      )
    } else { # after hackathon display:
      HTML('<p style="color: #5e9ca0; text-align: center;"><span style="color: #000000;">Thank you for participating in the Admiral Hackathon.</span><br /><span style="color: #000000;">Submissions are closed now.</span><br /><span style="color: #000000;">You can still upload your scripts for now, but they will not be registered.</span></p>
             <p style="color: #5e9ca0; text-align: center;"><span style="color: #000000;">---------------------------------------------------</span></p>')
    }
  })


  ## registration --------------------------------------------------------------
  registrationData <-
    reactive({
      N <- input$n_members
      NAME <- sapply(1:N, function(i) {
        paste0("name", i)
      })
      EMAIL <- sapply(1:N, function(i) {
        paste0("email", i)
      })
      names <- character(0)
      emails <- character(0)

      for (i in 1:N) {
        names[i] <- input[[paste0(NAME[i])]]
        emails[i] <- input[[paste0(EMAIL[i])]]
      }

      dplyr::tibble(
        team_name = input$team_name,
        n_members = N,
        member_name = names,
        member_email = emails
      )
    }) %>%
    bindCache(input$team_name, input$n_members, input$name1, input$email1) %>%
    bindEvent(input$register)
  registrationProcess <- reactive({
    req(registrationData())

    # register_team
    reg <- register_team(reg_dat = registrationData())
    reg
  })
  n_mem <- reactive({
    input$n_members
  })

  # registration feedback display
  dataModal <- function(failed = TRUE) {
    if (failed) {
      modalDialog(
        div(tags$b("Team registration was not successful.", style = "color: red;")),
        HTML(vectorBulletList(registrationProcess()$message)),
        footer = tagList(
          modalButton("OK")
        )
      )
    } else if (timer()$active) {
      modalDialog(
        {
          tagList(
            HTML(
              "<h1>Registration</h1>",
              "<p>Congratulations, your team --"
            ),
            tags$b(input$team_name),
            HTML("-- has successfully registered for the Admiral Hackathon.&nbsp;</p>")
          )
        },
        renderTable((registrationData())),
        footer = tagList(
          modalButton("OK")
        )
      )
    } else {
      modalDialog(
        {
          tagList(
            HTML(
              "<h1>Registration</h1>",
              "<p>The Hackathon has ended. &nbsp;</p>",
              "<p>Your team --"
            ),
            tags$b(input$team_name),
            HTML("-- has been added to the team selection.&nbsp;</p>")
          )
        },
        renderTable((registrationData())),
        footer = tagList(
          modalButton("OK")
        )
      )
    }
  }
  # call the modal when the register button is pressed
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

  # render email input UI of the register tab
  output$name_email <- shiny::renderUI({
    N <- n_mem()
    NAME <- sapply(1:N, function(i) {
      paste0("name", i)
    })
    EMAIL <- sapply(1:N, function(i) {
      paste0("email", i)
    })

    output <- tagList()

    firstsecondthird <- c("First", "Second", "Third", "Fourth", "Fifth")
    for (i in seq_along(1:N)) {
      output[[i]] <- tagList()
      output[[i]] <- fluidRow(
        shiny::h4(paste(firstsecondthird[i], " Member")),
        column(6,
          textInput(NAME[i], "Name"),
          value = " "
        ),
        column(6,
          textInput(EMAIL[i], "Email"),
          value = " "
        )
      )
    }
    output
  })

  ## upload script --------------------------------------------------------------
  # get team names from gdrive
  dirTeams <-
    reactive({
      input$register
      dir %>% drive_ls(type = "folder")
    })
  # render team names to select
  output$team_options <-
    renderUI({
      selectInput("team", "Choose team", dirTeams()$name)
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

    if (timer()$active) {
      out <- tryCatch(
        expr = {
          g_upload(
            r_file = inputData()$filepath,
            team = inputData()$team,
            task = inputData()$task,
            team_dir = inputData()$team_dir
          )
        },
        error = function(e) {
          message("Uploading to drive failed and caused the following error: ")
          safeError(e)
        },
        warning = function(w) {
          message("A Warning Occurred while uploading: ")
          w
        }
      )

      out$name
    }
  })

  output$frontpage <- shiny::renderText({
    # use https://onlinehtmleditor.dev/ to write html
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

  ## run script ----------------------------------------------------------------

  user_data <- shiny::reactive({
    req(inputData())

    # name of expected file to look for later
    expected_file <- task_df %>%
      filter(tasks == inputData()$task) %>%
      select(outcomes)

    # make required files available in the adam folder
    get_depends(
      task = inputData()$task,
      depends_list = depends_list,
      task_df = task_df,
      from = key,
      to = adam
    )

    # some participants got an error because Hmisc is creating a conflict.
    try(detach("package:Hmisc"))

    # source the user file
    tryCatch(
      expr = source(inputData()$filepath, local = new.env()),
      error = function(e) paste("while sourcing the script the following error appeared: ", safeError(stop(e)))
    )

    # get path to file
    path.to.expected <- list.files(adam, full.names = TRUE)[list.files(adam) %>%
      tolower() %in% {
        expected_file %>% tolower()
      }]

    if (length(path.to.expected) < 1) {
      stop(safeError(paste(expected_file, " not found. Perhaps you had not selected the correct task? Files found: ", list.files(adam))))
    }


    # this would upload the datafile to google
    # for(i in files){

    #   inputData()$team_dir %>% drive_upload(media = i, overwrite = TRUE)
    # }


    user_data <- tryCatch(haven::read_xpt(path.to.expected),
      error = function(e) stop(safeError(e))
    )


    # remove all files in adam folder for next participant
    unlink(list.files(adam, full.names = TRUE))

    user_data
  })

  # render script to preview
  output$script <- shiny::renderText({
    head(readr::read_file(inputData()$filepath))
  })

  ## user table ----------------------------------------------------------------
  output$table <- shiny::renderTable({
    req(inputData())

    tryCatch(
      expr = user_data() %>% head(),
      error = function(e) stop(safeError(e))
    )
  })


  ## compare files -------------------------------------------------------------
  # load relevant key data
  key_data <- shiny::reactive({
    # this should return the relevant adam datasets name
    expected_file <- task_df %>%
      filter(tasks == inputData()$task) %>%
      select(outcomes) %>%
      tolower()


    # read data from key folder
    haven::read_xpt(file.path(key, expected_file))
  })


  compare_df <- shiny::eventReactive(eventExpr = input$analyze, {
    req(inputData())
    req(user_data())
    req(key_data())
    difference <- tryCatch(
      expr = {
        compare_dfs(
          df_user = user_data(),
          df_key = key_data(),
          keys = key_list[[inputData()$task]]
        )
      },
      error = function(e) stop(safeError(e))
    )

    # send to google drive
    if (timer()$active) {
      tryCatch(
        expr = {
          score_to_google(
            score = difference$score,
            team = inputData()$team,
            task = inputData()$task
          )
        },
        error = function(e) stop(safeError(e))
      )
    }
    # output
    difference
  })
  # render the difference
  output$diffdf <- shiny::renderPrint({
    results <- tryCatch(
      {
        compare_df()$diffdf_result
      },
      error = function(e) stop(safeError(e))
    )



    results
  })


  # render the score
  output$score <- shiny::renderText({
    paste("Your Score was: ", compare_df()$score)
  })


  ## leaderboard ---------------------------------------------------------------
  # at the moment, this is not included in the output (to avoid competitiveness)
  output$summary <-
    shiny::renderTable({
      req(input$team)

      team_submissions <- get_leaderboard() %>%
        filter(team == input$team)
    #
    #   get_leaderboard(team)
    # task_info <-
    #     task_info <- tryCatch(
    #     expr = {
    #       tempdir <- tempfile(fileext = ".csv")
    #       dir %>%
    #         drive_ls() %>%
    #         filter(name == "task_info.csv") %>%
    #         drive_download(path = tempdir)
    #
    #       readr::read_csv(tempdir)
    #     },
    #     error = function(e) {
    #       NULL
    #     }
    #   )
    #
    #   if (!is.null(task_info)) {
    #     task_info <- task_info %>%
    #       select(-email) %>%
    #       filter(team == input$team) %>%
    #       group_by(team, task) %>%
    #       summarize(
    #         score = max(score)
    #       )
    #   } else {
    #     task_info <- tibble()
    #   }
    #
    #   task_info
    })
}
# Run the app ----
shinyApp(ui, server)
