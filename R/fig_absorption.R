cdom <- read_csv(here::here("data/clean/cdom_ultrapath.csv"))

cdom %>%
  filter(station_cast == 697 | station_cast == 620) %>%
  filter(depth == min(depth)) %>%
  filter(between(wav, 254, 600)) %>%
  ggplot(aes(x = wav, y = absorption, color = type)) +
  geom_line() +
  facet_wrap(~station_cast, scales = "free")

aphy <- vroom::vroom(here::here("data/clean/aphy.csv"))


p <- aphy %>%
  filter(station == "460") %>%
  # group_by(station) %>%
  # filter(pressure == min(pressure)) %>%
  filter(replicate != "B") %>%
  filter(method == "B") %>%
  filter(between(wavelength, 300, 800)) %>%
  ggplot(aes(x = wavelength, y = aphy_nm_1, group = spectra_id)) +
  geom_line() +
  facet_wrap(~station, scales = "free_y") +
  geom_vline(xintercept = 440)


# aphy %>%
#   filter(wavelength == 440 & replicate != "B") %>%
#   ggplot(aes(x = aphy_nm_1)) +
#   geom_histogram() +
#   scale_x_log10(labels = scales::label_number()) +
#   annotation_logticks(sides = "b")
