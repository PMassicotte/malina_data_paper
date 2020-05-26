df <- read_excel(
  "data/raw/Malina_data_compilation-Xie_DIC & CO photoproduction rate.xlsx",
  skip = 4,
  col_names = c(
    "station",
    "depth",
    "cutoff_wavelength_50_percent",
    "co2_production_moles_m3_s1",
    "co_production_moles_m3_s1"
  ),
  na = c("no data", "", " ")
)

df_viz <- df %>%
  filter(depth == "surface") %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(transect %in% c(300, 600)) %>%
  filter(cutoff_wavelength_50_percent %in% c(280, 295)) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

df_viz

mylabel <- c(
  "280" = "280 nm",
  "295" = "295 nm"
)

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p <- df_viz %>%
  pivot_longer(starts_with("co"), names_to = "type", values_to = "flux") %>%
  ggplot(aes(x = flux * 1e6, y = factor(station), fill = type)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_discrete(expand = expansion(mult = c(0, 0))) +
  facet_grid(
    transect ~ cutoff_wavelength_50_percent,
    scales = "free_y",
    labeller = labeller(cutoff_wavelength_50_percent = mylabel, transect = lab)
  ) +
  scale_fill_manual(
    guide = guide_legend(
      label.position = "top",
      label.theme = element_text(face = "bold", family = "Poppins", size = 8),
      keywidth = unit(4, "cm"),
      keyheight = unit(0.2, "cm")
    ),
    breaks = c("co2_production_moles_m3_s1", "co_production_moles_m3_s1"),
    labels = c(bquote("Carbon dioxide"~(CO[2])), bquote("Carbon monoxide"~(CO))),
    values = c("#A3BE8CFF", "#BF616AFF")
  ) +
  labs(
    x = bquote("Production rate"~("µmol"~m^{-3}~s^{-1})),
    y = "Station number"
  ) +
  theme(
    plot.caption = element_text(size = 4),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing.y = unit(2, "lines")
  )

paletteer::paletteer_d("nord::aurora")

ggsave("graphs/fig11.pdf",
  device = cairo_pdf,
  width = 17.5 / 1.5,
  height = 12,
  units = "cm"
)
