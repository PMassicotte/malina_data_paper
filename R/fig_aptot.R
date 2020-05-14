aptot <- vroom::vroom("data/clean/atot.csv")

aptot_viz <- aptot %>%
  filter(station %in% c("697", "620", "398", "320")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  mutate(transect = factor(transect, levels = c("600", "300"))) %>%
  group_by(station) %>%
  filter(pressure == min(pressure)) %>%
  ungroup()

aptot_viz

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

aptot_viz %>%
  ggplot(aes(x = wavelength, y = ap_nm_1, group = spectra_id)) +
  geom_line() +
  facet_wrap(~transect) +
  scale_y_log10()

aptot_viz %>%
  group_by(station, transect, wavelength) %>%
  summarise(ap_nm_1 = mean(ap_nm_1)) %>%
  ggplot(aes(x = wavelength, y = ap_nm_1, group = station)) +
  geom_line() +
  facet_wrap(~transect, labeller = labeller(transect = lab)) +
  scale_x_continuous(
    expand = expansion(mult = c(0.12, 0.05)),
    breaks = seq(250, 800, by = 50)
  ) +
  # coord_cartesian(xlim = c(250, 600), expand = TRUE) +
  # scale_y_continuous(
  #   expand = expansion(mult = c(0.01, 0.12)),
  #   breaks = scales::breaks_pretty(n = 4)
  # ) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  labs(
    x = "Wavelength (nm)",
    y = bquote(italic(a)["TOT"]~(m^{-1}))
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )
