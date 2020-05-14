ac9 <- read_csv("data/raw/csv/downcast.csv")

ac9 <- ac9 %>%
  pivot_longer(
    -c(filename, depth, pressure, serial_number, comments),
    names_to = "var",
    values_to = "values"
  ) %>%
  extract(
    var,
    into = c("var", "wavelength"),
    regex = "(bb_p|^a|^c|^beta)(\\d+)",
    convert = TRUE
  ) %>%
  pivot_wider(names_from = var, values_from = values)

ac9

# Should have 12 measures (12 wavelengths) per depth
ac9 %>%
  count(filename, depth) %>%
  verify(n == 12)

# How many station do we have?
ac9 %>%
  distinct(filename)

# Extract cast number from the file name
ac9 <- ac9 %>%
  mutate(cast = str_match(filename, "cast(\\d+)")[, 2]) %>%
  mutate(cast = parse_number(cast))

ac9


# Associate station number based on the cast number -----------------------

stations <- readxl::read_excel("data/raw/ctd_jens/data_per_depth_matchup.xlsx") %>%
  janitor::clean_names() %>%
  distinct(cast, station)

stations

# Few AC9 files do not have matching station number
ac9 %>%
  anti_join(stations) %>%
  distinct(filename)

ac9 <- ac9 %>%
  inner_join(stations)

# Get geographical coordinates
stations <- read_csv("data/clean/stations.csv") %>%
  distinct(station, longitude, latitude, transect) %>%
  group_by(station, transect) %>%
  summarise(across(everything(), mean))

stations %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point()

ac9 %>%
  anti_join(stations)

ac9 <- ac9 %>%
  inner_join(stations)

p <- ac9 %>%
  drop_na(c) %>%
  ggplot(aes(
    x = c,
    y = depth,
    color = factor(wavelength),
    group = wavelength
  )) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~filename, scales = "free")

write_csv(ac9, "data/clean/ac9.csv")
