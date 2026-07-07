# Setup of the ShinyApp <- keep this comment for RStudio ####

# Deployment ####
# To deploy, run: rsconnect::deployApp()
# Or use the blue button in the top right corner
# of this file in RStudio

## For installation on a server running shiny-server
# remotes::install_github("jmbuhr/ShinyLipids", 
#                         force = TRUE, dependencies = TRUE, upgrade = TRUE)

# Database connections ####
## uncomment this to read from a database.
## In this example it is on the same server this App is running on
# databaseConnection <- DBI::dbConnect(RPostgres::Postgres(),
#                                      dbname = "ldb",
#                                      host = "localhost",
#                                      port = 5432,
#                                      user = Sys.getenv("DB_USER"))

## Local database file (for development)
# path <- "inst/extdata/Sqlite.db"
# databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)

## Server database (reads password from .env)
env_file <- "/home/ubuntu/03_flask/.env"
if (file.exists(env_file)) {
  env <- readLines(env_file)
  db_pass <- sub("DB_PASSWORD=", "", grep("DB_PASSWORD", env, value = TRUE))
  databaseConnection <- DBI::dbConnect(RPostgres::Postgres(),
                                       dbname = "ldb",
                                       host = "localhost",
                                       port = 5432,
                                       user = Sys.getenv("DB_USER"),
                                       password = db_pass)
} else {
  # Fallback to local SQLite for development
  path <- "inst/extdata/Sqlite.db"
  databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
}

# Run App ####
pkgload::load_all()
run_app(db = databaseConnection)
