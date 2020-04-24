# Clean Jens' CTD barge profiles made in the estuary.

rm(list = ls())

matchup <- readxl::read_excel("data/raw/ctd_jens/Log-Barge-Malina-Leg2b-EndofCruise.xls",
  skip = 15
) %>%
  select(
    station = 1,
    ctd = 2,
    # latitude = 12,
    # longitude = 16,
    ref = 32
  ) %>%
  filter(str_detect(station, "^\\d{3}")) %>%
  separate(
    station,
    into = c("station", "cast"),
    sep = "-",
    fill = "right",
    convert = TRUE
  ) %>%
  separate_rows(ref) %>%
  drop_na(ref) %>%
  type_convert()


matchup

ac9_files <- fs::dir_ls("data/raw/ctd_jens/AC9_barge/AC9_barge/", glob = "*.txt")
ref <- parse_number(str_match(ac9_files, "_(\\d{3})_")[, 2])

setdiff(matchup$ref, ref)
setdiff(ref, matchup$ref)

ctd_barge <- vroom::vroom(ac9_files, col_select = 1:6, delim = "\t", id = "filename") %>%
  janitor::clean_names() %>%
  mutate(filename = basename(filename)) %>%
  select(filename, pres = pres_dbar, temp = temp_c, sal = sal_psu) %>%
  mutate(ref = str_match(filename, "_(\\d{3})_")[, 2]) %>%
  mutate(ref = parse_number(ref))

# Some profile have only 1-2 observations, I should remove them
ctd_barge %>%
  count(filename, sort = TRUE)

ctd_barge %>%
  add_count(filename) %>%
  filter(n >= 10) %>% # Keep profiles with at least 10 measurements
  ggplot(aes(x = sal, y = pres, group = filename)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~filename, scales = "free_y")

ctd_barge <- ctd_barge %>%
  add_count(filename) %>%
  filter(n >= 10) # Keep profiles with at least 10 measurements

# Get station information -------------------------------------------------

ctd_barge_merged <- ctd_barge %>%
  inner_join(matchup, by = "ref")

ctd_barge_merged

# Get long/lat information
stations <- read_csv("data/clean/stations.csv") %>%
  select(-cast) %>%
  group_by(station, transect) %>%
  summarise(across(everything(), mean)) %>%
  ungroup()

ctd_barge_merged <- ctd_barge_merged %>%
  inner_join(stations, by = "station")

write_csv(ctd_barge_merged, "data/clean/ctd_barge.csv")

ctd_rosette <- read_csv("data/clean/ctd.csv") %>%
  distinct(station, longitude = initial_longitude_deg, latitude = initial_latitude_deg)

new_stations <- anti_join(ctd_barge_merged, ctd_rosette, by = "station") %>%
  distinct(station, longitude, latitude)

p <- ctd_barge_merged %>%
  distinct(station, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point() +
  geom_point(data = new_stations, aes(x = longitude, y = latitude), color = "red") +
  ggrepel::geom_text_repel(aes(label = station)) +
  coord_map() +
  labs(
    title = "Positions of the barge CTD data",
    subtitle = "The red dot indicates the barge station that was not found in the rosette data"
  )

ggsave(
  "graphs/ctd_barge_vs_rosette_station_locations.pdf",
  device = cairo_pdf
)

# Compare CTD rosette and barge for station 680 ---------------------------

# Just to make sure that the data can be used together and is comparable. The
# barge data for station 680 is the ac9 cast file 139 (based on
# info_malina_Doxaran_IOP_16122010.doc.)

ctd_barge_station_680 <- ctd_barge %>%
  filter(str_detect(filename, "139")) %>%
  select(-filename) %>%
  mutate(source = "barge")

ctd_rosette_station_680 <- vroom::vroom("data/clean/ctd.csv") %>%
  filter(station == 680) %>%
  select(pres, sal, temp) %>%
  group_by(pres) %>%
  summarise(across(everything(), mean)) %>%
  mutate(source = "rosette")

ctd_merged <- bind_rows(ctd_barge_station_680, ctd_rosette_station_680)

ctd_merged %>%
  pivot_longer(cols = c("temp", "sal"), names_to = "variable", values_to = "value") %>%
  drop_na(value) %>%
  ggplot(aes(x = value, y = pres, color = source, group = source)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "Salinity and temperature vertical profiles at station 680",
    subtitle = "Compare CTD from barge and rosette, so we know they are comparable"
  )

ggsave(
  "graphs/ctd_barge_vs_rosette_station_680.pdf",
  device = cairo_pdf
)
