aphy <- read_csv("data/clean/aphy.csv", guess_max = 1e6) %>%
  filter(replicate != "A") %>%
  group_by(spectra_id) %>%
  filter(all(aphy_nm_1 != 0)) %>%
  ungroup()

aphy %>%
  distinct(replicate)

p <- aphy %>%
  filter(between(wavelength, 254, 800)) %>%
  ggplot(aes(x = wavelength, y = aphy_nm_1, group = spectra_id)) +
  geom_line(size = 0.1) +
  facet_wrap(~station, scales = "free_y") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))

ggsave("~/Desktop/test.pdf", device = cairo_pdf)
