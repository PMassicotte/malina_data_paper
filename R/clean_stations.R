rm(list = ls())

station <- read_csv("data/raw/csv/station.csv", locale = locale(encoding = "Latin1")) %>%
  mutate(longitude = -longitude) %>%
  mutate(date = str_replace_all(date, "\u009e", "u")) %>%
  mutate(month = case_when(
    str_detect(date, "juil") ~ 7,
    str_detect(date, "aout") ~ 8,
    TRUE ~ NA_real_
  )) %>%
  mutate(date = parse_number(date)) %>%
  mutate(date = as.Date(paste("2009", month, date), format = "%Y %m %d")) %>%
  select(-month, -depth, -time) %>%
  drop_na(station, cast)

station %>%
  count(station, sort = TRUE)

#TODO: Marcel told me there were missing stations... I will try to find
#"missing" stations in other files.

station2 <- read_csv("data/raw/csv/A.csv", guess_max = 1e5) %>%
  separate(date, into = c("day", "month", "year")) %>%
  mutate(month = case_when(
    str_detect(month, "juil") ~ 7,
    str_detect(month, "aout") ~ 8,
    TRUE ~ NA_real_
  )) %>%
  mutate(date = as.Date(paste("2009", month, day), format = "%Y %m %d")) %>%
  select(date, station, cast, latitude, longitude) %>%
  distinct(date, station, cast, longitude, latitude) %>%
  mutate(station = parse_number(station)) %>%
  filter(station >= 100) %>%
  drop_na(station, cast)

station2 %>%
  count(station, cast, sort = TRUE) %>%
  verify(n == 1)

station3 <- read_csv("data/raw/csv/bacterial.csv") %>%
  separate(date, into = c("day", "month", "year")) %>%
  mutate(month = case_when(
    str_detect(month, "juil") ~ 7,
    str_detect(month, "aout") ~ 8,
    TRUE ~ NA_real_
  )) %>%
  # mutate(date = parse_number(date)) %>%
  mutate(date = as.Date(paste("2009", month, day), format = "%Y %m %d")) %>%
  select(date, station, cast, latitude, longitude) %>%
  distinct(station, cast, longitude, latitude, .keep_all = TRUE) %>%
  drop_na(station, cast)

# Combine -----------------------------------------------------------------

station <- bind_rows(station, station2, station3)  %>%
  # drop_na(station, cast) %>%
  distinct(station, cast, .keep_all = TRUE) %>%
  mutate(transect = station %/% 100 * 100)

write_csv(station, "data/clean/stations.csv")

# Explore -----------------------------------------------------------------

station %>%
  count(cast, sort = TRUE)
