
# read data ---------------------------------------------------------------

library(outbreaks)
library(dplyr)
library(purrr)

# read data
dat <- outbreaks::mers_korea_2015 %>% 
  purrr::pluck("linelist") %>% 
  dplyr::as_tibble()

dat %>% dplyr::glimpse()

# assumption: in this dataset we have two tables: report and followup (death)
table_report <- dat %>% dplyr::select(id:dt_diag)
table_followup <- dat %>% dplyr::select(id,outcome,dt_death)

# database management -----------------------------------------------------

library(DBI)
library(RSQLite)

# Create a temporary SQLite database in memory
database_outbreak <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = ":memory:"
)

# Store the dataframes as a table for the database
# in the SQLite database
DBI::dbWriteTable(
  conn = database_outbreak,
  name = "report",
  value = table_report
)

DBI::dbWriteTable(
  conn = database_outbreak,
  name = "followup",
  value = table_followup
)

database_outbreak

# query data --------------------------------------------------------------

library(dplyr)
# library(dbplyr)

# Query data using dplyr verbs
database_report <- dplyr::tbl(database_outbreak, "report")
database_followup <- dplyr::tbl(database_outbreak, "followup")

database_report_query <- database_report %>%
  dplyr::select(id,age,sex,dt_onset,dt_report) %>% 
  filter(sex == "F")

# Show SQL query
database_report_query %>% 
  dplyr::show_query()

# Join tables
database_join_tables <- database_report_query %>%
  dplyr::left_join(database_followup)

# Collect query and join
database_collect <- database_join_tables %>%
  dplyr::collect()

database_collect

# # Read data from the 'cases' table
# result <- DBI::dbReadTable(
#   conn = database_con,
#   name = "cases"
# )

# close connection --------------------------------------------------------

# Previously created database objects - readable
database_outbreak
database_report
database_followup
database_join_tables

# Close the database connection
DBI::dbDisconnect(conn = database_outbreak)

# Previously created database objects - not readable
database_outbreak
database_report
database_followup
database_join_tables

