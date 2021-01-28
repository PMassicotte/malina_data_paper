# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Separate the data into multiple CSV files. As pointed out by
# reviewer 2, the original file contianing all the data was very big and
# difficult to explore.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

con <- dbConnect(RSQLite::SQLite(), dbname = "data/raw/Takuvik.sqlite")

tables <- dbListTables(con)

sqlite_to_csv <- function(table, con) {

  tbl(con, table) %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    write_csv(glue("data/raw/csv/{table}.csv"))

}

walk(tables, sqlite_to_csv, con = con)

# Check dates -------------------------------------------------------------

# Reviewer 2 said there were data measured in 2008 and so on that are not
# related to MALINA. Let's check it out.

files <- fs::dir_ls(here::here("data/raw/csv/"))

date_range <- function(file) {

  # file <- files[1]

  df <- read_csv(file) %>%
    janitor::clean_names() %>%
    select(contains("date")) %>%
    distinct()

  return(df)
}

res <- map(files, date_range)

res

# Why this file only contains Tara data? I will remove it from the list.
read_csv("/media/LaCie16TB/work/projects/malina/malina_data_paper/data/raw/csv/uvp5odv.csv") %>%
  distinct(cruise)
