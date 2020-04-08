ac9 <- read_csv("data/raw/csv/downcast.csv")

ac9 <- ac9 %>%
  pivot_longer(-c(filename, depth, pressure, serial_number, comments), names_to = "var", values_to = "values") %>%
  extract(var, into = c("var", "wavelength"), regex = "(bb_p|^a|^c|^beta)(\\d+)", convert = TRUE) %>%
  pivot_wider(names_from = var, values_from = values) %>%
  mutate(spectra_id = group_indices(., filename, depth))

# Should have 12 measures (12 wavelengths) per depth
ac9 %>%
  count(filename, depth) %>%
  verify(n == 12)

p <- ac9 %>%
  drop_na(c) %>%
  ggplot(aes(x = c, y = depth, color = factor(wavelength), group = wavelength)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~filename, scales = "free")

write_csv(ac9, "data/clean/ac9.csv")
