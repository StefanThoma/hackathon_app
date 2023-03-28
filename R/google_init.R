# establish connection to google -----------------------------------------------
## This sets token in project.
# # designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
#
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
#
# # trigger auth on purpose --> store a token in the specified cache
drive_auth()
gs4_auth(token = drive_token())


options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)


# find project drive
pattrn <- "mock_hackathon"
dir <- drive_find(pattern = pattrn, type = "folder", n_max = 1)

# first time: create google sheet to track events and move it into the right folder:
gs4_create(name = "event_info", sheets = c("teams", "submissions"))
googledrive::drive_mv("event_info", path = file.path(pattrn, "event_info"))
