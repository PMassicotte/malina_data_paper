rm(list = ls())

cops <- read_csv("data/raw/csv/hookermalina.csv")

cops <- cops %>%
  pivot_longer(
    cols = matches("\\d{3}$"),
    names_to = c(".value", "wavelength"),
    names_pattern = c("(.*)(\\d{3})")
  ) %>%
  mutate(wavelength = parse_number(wavelength))

cops <- cops %>%
  extract(station, into = c("station", NA), regex = "(\\d{3})(.*)", convert = TRUE)

# Check that we have 19 wavelengths at each station/cast
cops %>%
  count(station, cast, sort = TRUE) %>%
  assertr::verify(n == 19)

# No depths associated to the measurements?

cops %>%
  ggplot(aes(x = wavelength, y = es, group = filename)) +
  geom_line(size = 0.25) +
  facet_wrap(~station)
