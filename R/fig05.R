rm(list = ls())

station <- read_csv("data/clean/stations.csv") %>%
  distinct(station, cast, .keep_all = TRUE) %>%
  filter(transect %in% c(600, 300))

acdom <- read_csv("data/raw/csv/ultrapath.csv") %>%
  filter(str_detect(cast, "^\\d+")) %>%
  mutate(cast = parse_number(cast))

df <- left_join(station, acdom) %>%
  mutate(transect = factor(transect, levels = c("600", "300"))) %>%
  filter(station != 345) # This station is off the transect

df %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = station))

df %>%
  drop_na(depth) %>%
  ggplot(aes(x = latitude, y = acdom440_m_1, color = factor(transect))) +
  geom_point() +
  facet_wrap(~depth, scales = "free_y")

df %>%
  drop_na(depth) %>%
  filter(depth == 3) %>%
  ggplot(aes(x = latitude, y = acdom440_m_1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
  facet_wrap(~transect, scales = "free")
