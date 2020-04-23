rm(list = ls())

# Stations from the master file -------------------------------------------

# There are like 1.5K columns in this file...Read the first few since we are
# only interested in these.
stations <- readxl::read_excel(
  "data/raw/takuvik_excel.xlsx",
  range = cell_cols(1:27)
)

stations_master <- stations %>%
  select(matches("station|longitude|latitude|cast", ignore.case = TRUE)) %>%
  select(1:4) %>%
  set_names(c("station", "latitude", "longitude", "cast")) %>%
  distinct(.keep_all = TRUE) %>%
  drop_na() %>%
  filter(str_starts(station, "\\d{3}")) %>%
  mutate(station = str_match(station, "^\\d{3}")) %>%
  type_convert() %>%
  mutate(transect = station %/% 100 * 100)

stations_master

# Looks like some stations have bad coordinates
stations_master %>%
  # distinct(station, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(aes(color = factor(transect)))

stations_master <- stations_master %>%
  filter(longitude <= -50)

stations_master %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(aes(color = factor(transect)))

# Based on map, it seems that station 644 is not located at the right place.
stations_master <- stations_master %>%
  filter(station != 644)

# Station from the HPLC data ----------------------------------------------

# The HPLC data file contains stations that are not in the "master" data file. I
# will use this additional source of data to complete the "master" list.

stations_hplc <- readxl::read_excel("data/raw/malina-pig-20120918.xls") %>%
  janitor::clean_names() %>%
  mutate(across(is.character, .fns = ~ str_remove(., "LOD"))) %>%
  type_convert() %>%
  select(
    station,
    latitude = latitude_n,
    longitude = longitude_e,
    cast = ctd
  ) %>%
  filter(str_starts(station, "\\d{3}")) %>%
  mutate(station = str_match(station, "^\\d{3}")[, 1]) %>%
  mutate(cast = str_match(cast, "^\\d+")[, 1]) %>%
  type_convert() %>%
  mutate(transect = station %/% 100 * 100) %>%
  distinct() %>%
  filter(between(latitude, 65, 72) & longitude < -125) %>%
  filter(station != 345 & station != 437 & station != 235)

stations_hplc %>%
  # distinct(station, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(aes(color = factor(transect))) +
  ggrepel::geom_text_repel(aes(label = station))

# Combine -----------------------------------------------------------------

stations_clean <- bind_rows(stations_master, stations_hplc) %>%
  distinct()

# Write -------------------------------------------------------------------

write_csv(stations_clean, "data/clean/stations.csv")
