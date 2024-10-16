# Load packages
library(tidyverse) # for {dplyr} functions and the pipe %>%
library(rio) # for importing data

# read data
# e.g., the path to our file is data/raw-data/ebola_cases_2.csv then:
ebola_confirmed <- rio::import(
  "https://epiverse-trace.github.io/tutorials-early/data/ebola_cases_2.csv"
) %>%
  dplyr::as_tibble() # for a simple data frame output

# preview data
ebola_confirmed

# 1 connect with database -------------------------------------------------

library(DBI)
library(RSQLite)

# Create a temporary SQLite database in memory
db_connection <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  # host = "database.epiversetrace.com",
  # user = "juanito",
  # password = epiversetrace::askForPassword("Database password")
  dbname = ":memory:"
)

# 0 write a local data frame as a table in data base ----------------------

# Store the 'ebola_confirmed' dataframe as a table named 'cases'
# in the SQLite database
DBI::dbWriteTable(
  conn = db_connection,
  name = "cases",
  value = ebola_confirmed
)

# 2 read table from data base and wrangle output --------------------------

# Read one table from the database
mytable_db <- dplyr::tbl(src = db_connection, "cases")

# Show the SQL queries translated
mytable_db %>%
  dplyr::filter(confirm > 50) %>%
  dplyr::arrange(desc(confirm)) %>%
  dplyr::show_query()

# 3 extract data from data base -------------------------------------------

# Pull all data down to a local tibble
extracted_data <- mytable_db %>%
  dplyr::filter(confirm > 50) %>%
  dplyr::arrange(desc(confirm)) %>%
  dplyr::collect()

# 4 close database connection ---------------------------------------------

# Close the database connection
DBI::dbDisconnect(conn = db_connection)


# 5 work with extracted data downstream -----------------------------------

extracted_data
