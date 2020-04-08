
con <- dbConnect(RSQLite::SQLite(), dbname = "data/raw/Takuvik.sqlite")

tables <- dbListTables(con)

sqlite_to_csv <- function(table, con) {

  tbl(con, table) %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    write_csv(glue("data/raw/csv/{table}.csv"))

}

walk(tables, sqlite_to_csv, con = con)

