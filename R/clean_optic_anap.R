rm(list = ls())

anap <- read_csv("data/raw/csv/anap.csv") %>%
  select(-a, -snap) %>%
  rowid_to_column(var = "spectra_id") %>%
  pivot_longer(starts_with("anap"),
    names_to = "wavelength",
    values_to = "anap_nm_1"
  ) %>%
  mutate(wavelength = parse_number(wavelength)) %>%
  distinct() %>%
  group_by(spectra_id) %>%
  add_count() %>%
  verify(n == 501) %>%
  select(-n)

anap %>%
  drop_na(anap_nm_1) %>%
  filter(between(wavelength, 300, 600)) %>%
  ggplot(aes(x = wavelength, y = anap_nm_1, group = spectra_id)) +
  geom_line(size = 0.5, alpha = 0.2) +
  labs(
    title = "anap",
    subtitle = glue("Contains {n_distinct(anap$spectra_id)} spectra")
  ) +
  scale_y_log10()

write_csv(anap, "data/clean/anap.csv")

# 43 stations
anap %>%
  distinct(station)
