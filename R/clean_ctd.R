rm(list = ls())

read_header <- function(file) {
  dat <- read_lines(file, n_max = 11) %>%
    str_remove_all("% ")

  meta <- str_match(dat, "(.*):\\s+(.*)")[, 2:3] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    pivot_wider(names_from = V1, values_from = V2) %>%
    janitor::clean_names() %>%
    # type_convert() %>%
    mutate(
      start_date_time_utc = lubridate::parse_date_time(
        start_date_time_utc,
        orders = "dbYHMS"
      ),
      tz = "UTC"
    )

  return(meta)
}

read_ctd <- function(file) {
  metadata <- read_header(file)

  data <- read_table2(file, skip = 12, na = "NaN", col_types = cols(.default = col_number())) %>%
    slice(-1) %>%
    # type_convert() %>%
    janitor::clean_names()

  df <- add_column(data, metadata, .before = 1)

  return(df)
}

file <- "data/raw/ctd/0902_int/0902_054.int"
files <- fs::dir_ls("data/raw/ctd/0902_int/", glob = "*.int")

df <- map_df(files, read_ctd)

df <- df %>%
  filter(str_detect(station, "\\d{3}")) %>%
  mutate(station = parse_number(station)) %>%
  type_convert() %>%
  mutate(transect = station %/% 100 * 100)

df %>%
  ggplot(aes(x = initial_longitude_deg, y = initial_latitude_deg)) +
  geom_point()

data.table::fwrite(df, "data/clean/ctd.csv")
