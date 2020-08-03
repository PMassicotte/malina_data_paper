# Figure on DOM oxydation

rm(list = ls())

# Photo-oxydation ---------------------------------------------------------

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
  mutate(transect = factor(transect, levels = c("600", "300"))) %>%
  filter(cutoff_wavelength_50_percent == 295)

df_viz

# Unit conversion ---------------------------------------------------------

df_viz <- df_viz %>%
  mutate(across(starts_with("co"), ~ . * 60 * 60 / 1000 * 10^9,
    .names = "{str_match(col, 'co2?_production')}_nmol_l1_h1"
  ))

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p1 <- df_viz %>%
  select(!contains("m3_s1")) %>%
  pivot_longer(starts_with("co"), names_to = "type", values_to = "flux") %>%
  mutate(station = fct_reorder(as.character(station), station, .desc = TRUE)) %>%
  ggplot(aes(x = flux, y = station, fill = type)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_discrete(expand = expansion(mult = c(0, 0))) +
  facet_wrap(~transect,
    scales = "free_y",
    labeller = labeller(transect = lab)
  ) +
  scale_fill_manual(
    guide = guide_legend(
      label.position = "top",
      label.theme = element_text(face = "bold", family = "Poppins", size = 8),
      keywidth = unit(4, "cm"),
      keyheight = unit(0.2, "cm")
    ),
    breaks = c("co2_production_nmol_l1_h1", "co_production_nmol_l1_h1"),
    labels = c(bquote("Carbon dioxide"~(CO[2])), bquote("Carbon monoxide"~(CO))),
    values = c("#A3BE8CFF", "#BF616AFF")
  ) +
  labs(
    x = bquote("Production rate"~("nmol"~L^{-1}~h^{-1})),
    y = "Station"
  ) +
  theme(
    plot.caption = element_text(size = 4),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(hjust = 0, size = 10, face = "bold"),
    strip.text.y = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing.y = unit(2, "lines")
  )

paletteer::paletteer_d("nord::aurora")

# Autoxidation ------------------------------------------------------------

autoxidation <- read_excel("data/raw/new_data/Données autoxydation.xlsx") %>%
  janitor::clean_names() %>%
  janitor::remove_empty() %>%
  filter(!str_detect(station, fixed("station", ignore_case = TRUE))) %>%
  type_convert() %>%
  pivot_longer(-station, names_to = "process", values_to = "percent") %>%
  mutate(station = str_remove_all(station, "St ")) %>%
  mutate(percent = percent / 100) %>%
  mutate(process = str_replace_all(process, "_", " ")) %>%
  mutate(process = str_remove_all(process, " percent")) %>%
  mutate(process = str_to_sentence(process)) %>%
  mutate(transect = rep(c("300", "600"), each = 14)) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

autoxidation

autoxidation <- autoxidation %>%
  mutate(station_index = parse_number(station)) %>%
  mutate(station_index = ifelse(is.na(station_index), 999, station_index)) %>%
  mutate(station = fct_reorder2(station, station_index, transect, max))

autoxidation

p2 <- autoxidation %>%
  ggplot(aes(x = station, y = percent, fill = process)) +
  geom_col(position = "dodge") +
  facet_wrap(~transect, scales = "free_y", labeller = labeller(transect = lab)) +
  labs(
    x = "Station",
    y = NULL
  ) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  paletteer::scale_fill_paletteer_d("jcolors::pal5",
    guide = guide_legend(
      label.position = "top",
      label.theme = element_text(family = "Poppins", size = 8),
      keywidth = unit(4, "cm"),
      keyheight = unit(0.2, "cm")
    )
  ) +
  theme(
    plot.caption = element_text(size = 4),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing.y = unit(2, "lines")
  ) +
  coord_flip()


# Save --------------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 1, heights = c(0.5, 0.5)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave("graphs/fig13.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 18,
  units = "cm"
)
