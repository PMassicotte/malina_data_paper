# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Salinity/temperature plots.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

ctd_rosette <- fread("data/clean/ctd.csv") %>%
  filter(pres <= 100) %>%
  filter(transect %in% c(600, 300)) %>%
  as_tibble() %>%
  select(
    station,
    transect,
    latitude = initial_latitude_deg,
    longitude = initial_longitude_deg,
    pres,
    sal,
    temp
  ) %>%
  drop_na()

ctd_barge <- fread("data/clean/ctd_barge.csv") %>%
  filter(pres <= 100) %>%
  filter(transect %in% c(600, 300)) %>%
  as_tibble() %>%
  select(
    station,
    transect,
    latitude,
    longitude,
    pres,
    sal,
    temp
  ) %>%
  drop_na()

# Complete the dataset with barge data
ctd_missing <- ctd_barge %>%
  anti_join(ctd_rosette, by = c("station", "transect"))

df <- ctd_rosette %>%
  bind_rows(ctd_missing) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

df

df %>%
  count(transect)

df %>%
  distinct(station, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = station))

# Remove station 345
df <- df %>%
  filter(station != 345)

df %>%
  count(station, pres, sort = TRUE)

# Merge stations 689 and 690 ----------------------------------------------

# 689 and 690 are very close, I will merge them for the figure
df <- df %>%
  mutate(station = ifelse(station == 689, 690, station)) %>%
  group_by(station, transect, pres) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(station) %>%
  mutate(across(c("longitude", "latitude"), ~mean(.x, na.rm = TRUE))) %>%
  ungroup()

# Interpolation -----------------------------------------------------------

res <- df %>%
  group_nest(transect) %>%
  mutate(interpolated_temperature = map(
    data,
    interpolate_2d,
    x = latitude,
    y = pres,
    z = temp,
    n = 1,
    m = 1,
    h = 5
  )) %>%
  mutate(interpolated_salinity = map(
    data,
    interpolate_2d,
    x = latitude,
    y = pres,
    z = sal,
    n = 1,
    m = 1,
    h = 5
  ))

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

# Temperature -------------------------------------------------------------

p1 <- res %>%
  unnest(interpolated_temperature) %>%
  select(transect, x, y, z) %>%
  drop_na(z) %>%
  ggplot(aes(x = x, y = y, z = z, fill = z)) +
  geom_isobands(color = NA, breaks = seq(-10, 10, by = 0.25)) +
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
    data = unnest(res, data),
    aes(x = latitude, y = pres),
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
  paletteer::scale_fill_paletteer_c("oompaBase::jetColors",
  breaks = scales::breaks_pretty(n = 6),
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
    fill = expression("Temperature" ~ (degree * C))
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom"
  )

# Salinity ----------------------------------------------------------------

p2 <- res %>%
  unnest(interpolated_salinity) %>%
  select(transect, x, y, z) %>%
  drop_na(z) %>%
  ggplot(aes(x = x, y = y, z = z, fill = z)) +
  geom_isobands(color = NA, breaks = seq(-10, 60, by = 0.25)) +
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
    data = unnest(res, data),
    aes(x = latitude, y = pres),
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
  paletteer::scale_fill_paletteer_c("oompaBase::jetColors",
    breaks = scales::breaks_pretty(n = 6),
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
    fill = "Salinity (PSU)"
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

# Save plot ---------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave("graphs/fig04.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 14,
  units = "cm"
)
