rm(list = ls())

# Atot --------------------------------------------------------------------

rm(list = ls())

atot <- read_csv("data/raw/csv/aptot.csv") %>%
  rowid_to_column(var = "spectra_id") %>%
  pivot_longer(starts_with("ap"), names_to = "wavelength", values_to = "ap_nm_1") %>%
  mutate(wavelength = parse_number(wavelength))

write_csv(atot, "data/clean/atot.csv")

atot %>%
  count(spectra_id, sort = TRUE)

atot %>%
  # mutate(transect = station %/% 100 * 100) %>%
  # filter(spectra_id == 276) %>%
  ggplot(aes(x = wavelength, y = ap_nm_1, group = spectra_id)) +
  geom_line() +
  labs(
    title = "Atot",
    subtitle = glue("Contains {n_distinct(atot$spectra_id)} spectra")
  ) +
  facet_wrap(~station, scales = "free_y")


# Is there a latitudinal gradient?
atot %>%
  filter(str_starts(station, "6")) %>%
  filter(station != "696") %>%
  filter(wavelength %in% c(440, 678)) %>%
  group_by(station, wavelength) %>%
  summarise(mean_ap = mean(ap_nm_1), sd_ap = sd(ap_nm_1)) %>%
  ungroup() %>%
  ggplot(aes(x = station, y = mean_ap)) +
  geom_pointrange(aes(ymin = mean_ap - sd_ap, ymax = mean_ap + sd_ap)) +
  facet_wrap(~wavelength)
