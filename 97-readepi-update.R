# pak::pak("epiverse-trace/readepi@readepi_no_his_spc_deps")

library(readepi)
library(dplyr)
library(dbplyr)

# MySQL ------------------------------------------------------------------

# CONNECT TO THE TEST MYSQL SERVER
login <- readepi::login(
  from = "mysql-rfam-public.ebi.ac.uk",
  type = "MySQL",
  user_name = "rfamro",
  password = "",
  driver_name = "",
  db_name = "Rfam",
  port = 4497
)

# DISPLAY THE LIST OF TABLES FROM A DATABASE OF INTEREST
tables <- readepi::show_tables(login = login)


# interoperability with DBPLYR -------------------------------------------

author <- login %>% 
  dplyr::tbl(from = "author") %>% 
  dplyr::select(author_id, name)

family_author <- login %>% 
  dplyr::tbl(from = "family_author") %>% 
    dplyr::select(author_id, rfam_acc)

dplyr::left_join(author, family_author,keep = TRUE) %>% 
  dplyr::show_query()

dplyr::left_join(author, family_author,keep = TRUE) %>% 
  dplyr::collect()


# RDBMS ------------------------------------------------------------------


# READING ALL FIELDS AND ALL RECORDS FROM ONE TABLE (`author`) USING AN SQL QUERY
dat <- readepi::read_rdbms(
  login = login,
  query = "select * from author"
)

# SELECT FEW COLUMNS FROM ONE TABLE AND LEFT JOIN WITH ANOTHER TABLE
dat <- readepi::read_rdbms(
    login = login,
    query = "select author.author_id, author.name,
  family_author.author_id from author left join family_author on
  author.author_id = family_author.author_id"
)

tibble::as_tibble(dat,.name_repair = "unique")

# READING ALL FIELDS AND ALL RECORDS FROM ONE TABLE (`author`) WHERE QUERY PARAMETERS ARE SPECIFIED AS A LIST
dat <- read_rdbms(
  login = login,
  query = list(table = "author", fields = NULL, filter = NULL)
)

tibble::as_tibble(dat)


readepi::read_rdbms(
  login = login,
  query = "
  SELECT
  `author`.`author_id` AS `author_id.x`,
  `name`,
  `family_author`.`author_id` AS `author_id.y`,
  `rfam_acc`
FROM `author`
LEFT JOIN `family_author`
  ON (`author`.`author_id` = `family_author`.`author_id`)
  "
    ) %>% dplyr::as_tibble()

# TYPE: DHIS2 ------------------------------------------------------------

library(readepi)

# CONNECT TO A DHIS2 INSTANCE
dhis2_login <- readepi::login(
  type = "dhis2",
  from = "https://smc.moh.gm/dhis",
  user_name = "test",
  password = "Gambia@123"
)

# ?readepi::show_tables() ONLY WORK for mySQL not DHIS2

# IMPORT DATA FROM DHIS2 FOR THE SPECIFIED ORGANISATION UNIT AND PROGRAM IDs

tictoc::tic()
data <- readepi::read_dhis2(
  login = dhis2_login,
  org_unit = "GcLhRNAFppR",
  program = "E5IUQuHg3Mg"
)
tictoc::toc()

tibble::as_tibble(data)

# again

readepi::get_programs(login = dhis2_login)

org_units <- readepi::get_organisation_units(login = dhis2_login)
org_units


# import data from DHIS2 using names
data <- readepi::read_dhis2(
  login = dhis2_login,
  org_unit = "Keneba",
  program = "Child Registration & Treatment "
)


tibble::as_tibble(data)

# get the list of organisation units that run the program "E5IUQuHg3Mg"
target_org_units <- readepi::get_program_org_units(
  login = dhis2_login,
  program = "E5IUQuHg3Mg",
  org_units = org_units
)

tibble::as_tibble(target_org_units)



# SORMAS -----------------------------------------------------------------

# get the list of all disease names
sormas_login <- readepi::login(
  type = "sormas",
  from = "https://demo.sormas.org/sormas-rest",
  user_name = "SurvSup",
  password = "Lk5R7JXeZSEc"
)

# readepi::show_tables(disease_names)

tictoc::tic()
covid_cases <- readepi::read_sormas(
  login = sormas_login,
  disease = "coronavirus",
)
tictoc::toc()

tibble::as_tibble(covid_cases)

# tibble::as_tibble(disease_names)

readepi::sormas_get_diseases(login = sormas_login)
