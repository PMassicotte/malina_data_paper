rm(list = ls())

# There are no measurements for transect 300.
# Hi Philippe, I did many profiles during that transects, but unfortunately I
# made a mistake with the logger, so nothing was recorded. It sucked. So what
# you have is all the data that there is. Jens

ac9 <- vroom::vroom("data/clean/ac9.csv") %>%
  filter(transect %in% c(600, 300)) %>%
  filter(station != 345)

ac9 %>%
  drop_na(a) %>%
  filter(pressure <= 200) %>%
  ggplot(aes(
    x = a,
    y = pressure,
    group = interaction(cast, station),
    color = factor(wavelength)
  )) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~ cast + wavelength, ncol = 9, scales = "free")

ac9 %>%
  filter(pressure <= 10) %>%
  group_by(station, latitude, wavelength) %>%
  summarise(across(a:bb_p, ~mean(., na.rm = TRUE))) %>%
  ggplot(aes(x = latitude, y = bb_p)) +
  geom_line() +
  facet_wrap(~wavelength, scales = "free_y")

ac9 %>%
  group_by(filename, cast) %>%
  filter(depth == min(depth)) %>%
  ungroup() %>%
  # filter(cast == 88 & station == 680) %>%
  drop_na(c) %>%
  ggplot(aes(x = wavelength, y = c)) +
  geom_line() +
  facet_wrap(~cast + station, scales = "free_y")
