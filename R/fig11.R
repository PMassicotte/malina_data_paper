# Figure on pico and nano-eukaryotesaryotes

rm(list = ls())

source("R/interpolate_fun.R")


# Cytometry (abundance) ---------------------------------------------------

df <- read_excel("data/raw/MALINA flow cytometry 2020 with metadata.xlsx") %>% # DV
  # mutate(date = as.Date(date, "%d-%b-%y")) %>%  #DV
  select(station, cast = ctd, depth, pico_ml, nano_ml, latitude, longitude) %>% # DV
  filter(depth <= 100) %>%
  filter(station != 345) %>%
  drop_na(pico_ml, nano_ml) # DV

df

# Get data for which we have a station number

df <- df %>%
  filter(str_detect(station, "^\\d{3}$")) %>%
  type_convert() %>%
  mutate(transect = floor(station / 100) * 100) # DV

df

cyto <- df %>%
  filter(transect %in% c(300, 600)) %>%
  filter(station %in% c(300:390, 600:690)) # DV to remove the MacKenzie surface stations

# We have 1 "duplicate". I will average the data.
cyto %>%
  count(station, depth, sort = TRUE)

cyto %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

cyto <- cyto %>%
  group_by(station, cast, depth) %>%
  summarise(across(everything(), mean), n = n()) %>%
  ungroup() %>%
  mutate(transect = factor(transect, c("600", "300")))

# ---------------
# Pico-eukaryotes
# ---------------

# Interpolation -----------------------------------------------------------

res_pico <- cyto %>%
  group_nest(transect) %>%
  mutate(interpolated_pico_ml = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = pico_ml,
    n = 1,
    m = 1,
    h = 5
  ))

res_pico

# Plot --------------------------------------------------------------------

station_labels <- res_pico %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p_pico <- res_pico %>%
  unnest(interpolated_pico_ml) %>% # DV
  select(-data) %>%
  drop_na(z) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(0, 5000, by = 100)) + # DV
  geom_text(
    data = station_labels,
    aes(x = latitude, y = 0, label = station),
    inherit.aes = FALSE,
    size = 1.5,
    angle = 45,
    hjust = -0.1,
    color = "gray50"
  ) +
  geom_point(
    data = unnest(res_pico, data),
    aes(x = latitude, y = depth),
    size = 0.05,
    color = "gray50",
    inherit.aes = FALSE
  ) +
  facet_wrap(~transect, scales = "free_x", labeller = labeller(transect = lab)) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.15))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    breaks = scales::breaks_width(width = 1000),
    labels = scales::label_number(), # DV
    guide =
      guide_colorbar(
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  labs(
    x = "Latitude",
    y = "Depth (m)",
    fill = bquote("Pico-eukaryotes" ~ ("cells" ~ "mL"^{-1}))
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(r = 2),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

p_pico

# ---------------
# Nano-eukaryotes
# ---------------

# Interpolation -----------------------------------------------------------

res_nano <- cyto %>%
  group_nest(transect) %>%
  mutate(interpolated_nano_ml = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = nano_ml,
    n = 1,
    m = 1,
    h = 5
  ))

res_nano

# Plot --------------------------------------------------------------------

station_labels <- res_nano %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p_nano <- res_nano %>%
  unnest(interpolated_nano_ml) %>% # DV
  select(-data) %>%
  drop_na(z) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(0, 5000, by = 100)) + # DV
  geom_text(
    data = station_labels,
    aes(x = latitude, y = 0, label = station),
    inherit.aes = FALSE,
    size = 1.5,
    angle = 45,
    hjust = -0.1,
    color = "gray50"
  ) +
  geom_point(
    data = unnest(res_nano, data),
    aes(x = latitude, y = depth),
    size = 0.05,
    color = "gray50",
    inherit.aes = FALSE
  ) +
  facet_wrap(~transect, scales = "free_x", labeller = labeller(transect = lab)) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.15))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    breaks = scales::breaks_width(width = 1000),
    labels = scales::label_number(), # DV
    guide =
      guide_colorbar(
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  labs(
    x = "Latitude",
    y = "Depth (m)",
    fill = bquote("Nano-eukaryotes" ~ ("cells" ~ "mL"^{-1}))
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(r = 2),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

p_nano

# Save --------------------------------------------------------------------

p <- (p_pico / p_nano) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))


ggsave(
  "graphs/fig11.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 14,
  units = "cm"
)
