rm(list = ls())

files <- fs::dir_ls("data/raw/csv/")

# file <- files[[1]]
# file <- "data/raw/csv/hookerbarge.csv"

extract_station <- function(file) {
  df <- fread(file) %>%
    janitor::clean_names() %>%
    select(any_of(c(
      "station", "cast", "date", "longitude", "latitude"
    ))) %>%
    mutate_at(vars(any_of(c("station"))), ~str_match(., "^\\d*")[, 1]) %>%
    mutate_at(vars(any_of(
      c("station", "cast", "longitude", "latitude")
    )), as.numeric) %>%
    as_tibble()

  # Try to guess the date format

  df <- df %>%
    # some dates do not have the years, ex.: 07-aout
    mutate_at(.vars = vars(one_of("date")), .funs = function(x) {
      date <- case_when(
        str_count(x, "-") == 1 ~ paste(x, "-2009"),
        TRUE ~ x
      )

      return(date)
    }) %>%
    mutate_at(.vars = vars(one_of("date")), ~ str_replace_all(., "ž", "u")) %>%
    mutate_at(.vars = vars(one_of("date")), ~ str_replace_all(., "juil", "07")) %>%
    mutate_at(.vars = vars(one_of("date")), ~ str_replace_all(., "aout|aug", "08")) %>%
    # slice(1:10) %>%
    mutate_at(.vars = vars(one_of("date")), ~ lubridate::parse_date_time(., orders = c("dmy", "mdy"))) %>%
    mutate_at(.vars = vars(one_of("date")), as.Date)

  # Use negative longitudes
  df <- df %>%
    mutate_at(.vars = vars(one_of("longitude")), ~ ifelse(. > 0, -., .))


  return(df)
}

df <- map_df(files, extract_station) %>%
  distinct(station, cast, longitude, latitude) %>%
  drop_na() %>%
  filter(station >= 100) %>%
  mutate(transect = station %/% 100 * 100)

# Manual clean-up ---------------------------------------------------------

# Based on map, it seems that station 644 is not located at the right place.
df <- df %>%
  filter(station != 644)

write_csv(df, "data/clean/stations.csv")
