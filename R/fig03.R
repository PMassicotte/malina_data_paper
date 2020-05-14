rm(list = ls())

source("R/interpolate_fun.R")

df <-
  read_excel("data/raw/Data MALINA pour Philippe Massicotte.xls") %>%
  janitor::clean_names() %>%
  select(
    station,
    date,
    longitude = long_w,
    latitude = lat_n,
    depth = depth_m,
    contains("percent")
  ) %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(transect %in% c(300, 600)) %>%
  mutate(transect = factor(transect, levels = c("600", "300"))) %>%
  filter(depth <= 250) %>%
  filter(station != 670)

df

df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")


# Pivot longer ------------------------------------------------------------

# Pivot the dataframe and remove cdbw data since it is almost 0% contribution.

df <- df %>%
  pivot_longer(
    contains("percent"),
    names_to = "water_type",
    values_to = "percent"
  ) %>%
  filter(!str_detect(water_type, "cdbw"))

# Interpolation -----------------------------------------------------------

res <- df %>%
  group_nest(transect, water_type) %>%
  mutate(interpolated_percent = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = percent,
    n = 1,
    m = 1,
    h = 4
  ))

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

# Plot --------------------------------------------------------------------

unique(df$water_type)

lab <- c(
  "mw_percent" = "Mackenzie\nriver",
  "sim_percent" = "Sea-ice\nmeltwater",
  "pml_percent" = "Winter polar\nmixed water",
  "uhl_percent" = "Upper halocline water\n(Pacific water)",
  "lhl_percent" = "Lower halocline water\n(Atlantic water)"
)

facet_lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p <- res %>%
  select(-data) %>%
  unnest(interpolated_percent) %>%
  drop_na(z) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  mutate(z = ifelse(z > 100, 100, z)) %>%
  mutate(water_type2 = case_when(
    water_type == "sim_percent" ~ "Sea-ice\nmeltwater",
    water_type == "mw_percent" ~ "Mackenzie\nriver",
    water_type == "pml_percent" ~ "Winter polar\nmixed water",
    water_type == "uhl_percent" ~ "Upper halocline water\n(Pacific water)",
    water_type == "lhl_percent" ~ "Lower halocline water\n(Atlantic water)"
  )) %>%
  mutate(water_type2 = factor(water_type2,
    levels = c(
      "Sea-ice\nmeltwater",
      "Mackenzie\nriver",
      "Winter polar\nmixed water",
      "Upper halocline water\n(Pacific water)",
      "Lower halocline water\n(Atlantic water)"
    )
  )) %>%
  ggplot(aes(x = x, y = y, z = z, fill = z)) +
  geom_raster() +
  geom_isobands(color = NA, breaks = seq(-10, 110, by = 5)) +
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
    aes(x = latitude, y = depth),
    size = 0.05,
    color = "gray50",
    inherit.aes = FALSE
  ) +
  facet_grid(
    water_type2 ~ transect,
    scales = "free_x",
    labeller = labeller(transect = facet_lab)
  ) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.15))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c("oompaBase::jetColors",
    # breaks = seq(0, 120, by = 20),
    # limits = c(0, 100),
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
    fill = "Fraction (%)"
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(hjust = 0, size = 14, face = "bold"),
    strip.text.y = element_text(hjust = 0.5, size = 8),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  "graphs/fig03.pdf",
  width = 17.5,
  height = 17.5,
  device = cairo_pdf,
  units = "cm"
)
