# Figure with nutrients.

rm(list = ls())

source("R/interpolate_fun.R")

station <- read_csv("data/clean/stations.csv") %>%
  distinct(station, cast, .keep_all = TRUE)

nutrient <- read_csv("data/raw/csv/novembre.csv") %>%
  janitor::clean_names()

# Have a look to the localization of the nutrients
station %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point() +
  geom_point(
    data = distinct(nutrient, longitude, latitude),
    color = "red"
  )

 # ggrepel::geom_text_repel(aes(label = station))

# Get the station information
df <- nutrient %>%
  select(cast, bottle, depth, no3_30028_u_m, po4_30031_u_m) %>%
  inner_join(station, by = "cast") %>%
  filter(transect %in% c(600, 300)) %>%
  filter(depth <= 100) %>%
  mutate(transect = factor(transect, levels = c("600", "300"))) %>%
  filter(station != 345)

df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x") +
  labs(
    title = "Should we average measure at the same depth?",
    subtitle = "Check station 345 for example. It is because there are many cast for this station."
  )

# At station 345, there are 3 measures, probable 3 casts... I will average them
# (also the latitude, longitude positions).
df %>%
  count(station, depth, sort = TRUE)

# Average by station/depth ------------------------------------------------

df

df <- df %>%
  group_by(transect, station, depth) %>%
  summarise(across(
    c("no3_30028_u_m", "po4_30031_u_m", "longitude", "latitude"),
    .fns = ~ mean(.x, na.rm = TRUE)
  ), n = n()) %>%
  ungroup() %>%
  # Just make sure that latitudes line up
  group_by(station) %>%
  mutate(latitude = mean(latitude)) %>%
  ungroup()

df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x") +
  ggrepel::geom_text_repel(aes(label = station))

# Interpolate -------------------------------------------------------------

res <- df %>%
  group_nest(transect) %>%
  mutate(interpolated_no3 = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = no3_30028_u_m,
    n = 1,
    m = 1,
    h = 4
  )) %>%
  mutate(interpolated_po4 = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = po4_30031_u_m,
    n = 1,
    m = 1,
    h = 4
  ))

# Plot --------------------------------------------------------------------

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE)

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p1 <- res %>%
  unnest(interpolated_no3) %>%
  select(-data, -interpolated_po4) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  drop_na(z) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(-1, 200, by = 0.5)) +
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
    fill = bquote(NO[3]~("µmol"~L^{-1}))
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

p2 <- res %>%
  unnest(interpolated_po4) %>%
  select(-data, -interpolated_no3) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  drop_na(z) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(-1, 200, by = 0.05)) +
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
  facet_wrap(~transect, scales = "free_x", labeller = labeller(transect = lab)) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.15))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c("oompaBase::jetColors",
    # option = "B",
    # direction = -1,
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
    fill = bquote(PO[4]~("µmol"~L^{-1}))
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
  plot_annotation(tag_levels = "A")

ggsave(
  "graphs/fig05.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 14,
  units = "cm"
)

