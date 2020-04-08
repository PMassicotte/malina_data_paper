# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Salinity/temperature plots.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

df <- fread("data/clean/ctd.csv") %>%
  filter(pres <= 100) %>%
  filter(transect %in% c(600, 300)) %>%
  as_tibble() %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

df

df %>%
  count(transect)

df %>%
  group_by(initial_longitude_deg, initial_latitude_deg) %>%
  filter(pres == min(pres)) %>%
  ggplot(aes(x = initial_longitude_deg, y = initial_latitude_deg)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = station))

# Remove station 345 and only keep 1 station when there are more than 1
df <- df %>%
  filter(station != 345) %>%
  group_by(station) %>%
  filter(start_date_time_utc == min(start_date_time_utc)) %>%
  ungroup()

df %>%
  group_by(initial_longitude_deg, initial_latitude_deg) %>%
  filter(pres == min(pres)) %>%
  ggplot(aes(x = initial_longitude_deg, y = initial_latitude_deg)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = station))

# Interpolation -----------------------------------------------------------

res <- df %>%
  group_nest(transect) %>%
  mutate(interpolated_temperature = map(
    data,
    interpolate_2d,
    x = initial_latitude_deg,
    y = pres,
    z = temp
  )) %>%
  mutate(interpolated_salinity = map(
    data,
    interpolate_2d,
    x = initial_latitude_deg,
    y = pres,
    z = sal
  ))

# Temperature -------------------------------------------------------------

p1 <- res %>%
  unnest(interpolated_temperature) %>%
  drop_na(z) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(-10, 10, by = 0.25)) +
  geom_point(
    data = unnest(res, data),
    aes(x = initial_latitude_deg, y = pres),
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
    fill = expression("T" ~ (degree * C))
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank()
  )

# Salinity ----------------------------------------------------------------

p2 <- res %>%
  unnest(interpolated_salinity) %>%
  drop_na(z) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  ))  +
  geom_isobands(color = NA, breaks = seq(-10, 60, by = 0.5)) +
  geom_point(
    data = unnest(res, data),
    aes(x = initial_latitude_deg, y = pres),
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
    guide =
      guide_colorbar(
        barwidth = unit(0.5, "cm"),
        barheight = unit(4, "cm")
      )
  ) +
  labs(
    x = "Latitude",
    y = "Depth (m)",
    fill = "Salinity"
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold")
  )

# Save plot ---------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 1)

ggsave("graphs/fig03.pdf",
  device = cairo_pdf,
  width = 7,
  height = 5
)
