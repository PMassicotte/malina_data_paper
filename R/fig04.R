# Figure with nutrients.

rm(list = ls())

source("R/interpolate_fun.R")

station <- read_csv("data/clean/stations.csv")

nutrient <- read_csv("data/raw/csv/novembre.csv") %>%
  janitor::clean_names()

# Have a look to the localisation of the nutrients
station %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point() +
  geom_point(
    data = distinct(nutrient, longitude, latitude),
    color = "red"
  )

# Get the station information
df <- nutrient %>%
  select(cast, bottle, depth, no3_30028_u_m, po4_30031_u_m) %>%
  inner_join(station, by = "cast") %>%
  filter(transect %in% c(600, 300)) %>%
  filter(depth <= 100) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

df

res <- df %>%
  group_nest(transect) %>%
  mutate(interpolated_no3 = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = no3_30028_u_m
  )) %>%
  mutate(interpolated_po4 = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = po4_30031_u_m
  ))


# Plot --------------------------------------------------------------------

p1 <- res %>%
  unnest(interpolated_no3) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  drop_na(z) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(-1, 200, by = 0.5)) +
  geom_point(
    data = unnest(res, data),
    aes(x = latitude, y = depth),
    size = 0.05,
    color = "#3c3c3c",
    inherit.aes = FALSE
  ) +
  facet_wrap(~transect, scales = "free_x") +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.01)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  scale_fill_viridis_c(
    option = "B",
    direction = -1,
    guide =
      guide_colorbar(
        barwidth = unit(0.5, "cm"),
        barheight = unit(4, "cm")
      )
  ) +
  labs(
    x = "Latitude",
    y = "Depth (m)",
    fill = bquote(NO[3])
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank()
  )

p2 <- res %>%
  unnest(interpolated_po4) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  drop_na(z) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(-1, 200, by = 0.05)) +
  geom_point(
    data = unnest(res, data),
    aes(x = latitude, y = depth),
    size = 0.05,
    color = "#3c3c3c",
    inherit.aes = FALSE
  ) +
  facet_wrap(~transect, scales = "free_x") +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.01)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  scale_fill_viridis_c(
    option = "B",
    direction = -1,
    guide =
      guide_colorbar(
        barwidth = unit(0.5, "cm"),
        barheight = unit(4, "cm")
      )
  ) +
  labs(
    x = "Latitude",
    y = "Depth (m)",
    fill = bquote(PO[4])
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank()
  )

# Save plot ---------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 1)

ggsave(
  "graphs/fig04.pdf",
  device = cairo_pdf,
  width = 7,
  height = 5
)
