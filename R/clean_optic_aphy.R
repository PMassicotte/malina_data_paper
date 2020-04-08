rm(list = ls())

# Aphy --------------------------------------------------------------------

aphy <- read_csv("data/raw/csv/aphy.csv") %>%
  rowid_to_column(var = "spectra_id") %>%
  pivot_longer(starts_with("aphy"), names_to = "wavelength", values_to = "aphy_nm_1") %>%
  mutate(wavelength = parse_number(wavelength))

aphy %>%
  ggplot(aes(x = wavelength, y = aphy_nm_1, group = spectra_id)) +
  geom_line() +
  labs(
    title = "Aphy",
    subtitle = glue("Contains {n_distinct(aphy$spectra_id)} spectra")
  )

write_csv(aphy, "data/clean/aphy.csv")

# Is there a latitudinal gradient?

labels_aphy <- c(
  "440" = "aphy[440]",
  "678" = "aphy(678)"
)

labels_transect <- c(
  "100" = "Transect 100",
  "600" = "Transect 600"
)

aphy %>%
  filter(str_starts(station, "6")) %>%
  type_convert() %>%
  mutate(transect = station %/% 100 * 100) %>%
  # filter(station != "696") %>%
  filter(wavelength %in% c(440)) %>%
  group_by(station, wavelength, transect) %>%
  summarise(mean_ap = mean(aphy_nm_1), sd_ap = sd(aphy_nm_1)) %>%
  ungroup() %>%
  drop_na() %>%
  ggplot(aes(x = as.factor(station), y = mean_ap)) +
  geom_point() +
  # geom_line() +
  xlab("Station") +
  ylab(bquote(a[phy](440)~(m^{-1}))) +
  labs(
    title = "Latitudinal variation of aphy",
    subtitle = "Stations 620 -> 697 is north -> south"
  )

ggsave("graphs/fig03.pdf", device = cairo_pdf, width = 12, height = 8, units = "cm")
