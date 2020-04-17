rm(list = ls())

source("R/interpolate_fun.R")

df <- read_csv("data/raw/csv/bacterial.csv") %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(transect %in% c(300, 600)) %>%
  filter(depth <= 100) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

df

df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  facet_wrap(~transect, scales = "free_x") +
  scale_y_reverse()

# Interpolation -----------------------------------------------------------

res <- df %>%
  group_nest(transect) %>%
  mutate(interpolated_bp = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = bp_pmol_leu_l_1_h_1
  ))

# Plot --------------------------------------------------------------------

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

p <- res %>%
  unnest(interpolated_bp) %>%
  select(-data) %>%
  drop_na(z) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(0, 200, by = 5)) +
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
    color = "#3c3c3c",
    inherit.aes = FALSE
  ) +
  facet_wrap(~transect, scales = "free_x") +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.1))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  scale_fill_viridis_c(
    option = "B",
    direction = -1,
    guide =
      guide_colorbar(
        barwidth = unit(0.5, "cm"),
        barheight = unit(4, "cm")
      ),
    breaks = scales::breaks_pretty(n = 6)
  ) +
  labs(
    x = "Latitude",
    y = "Depth (m)",
    fill = bquote(BP~(pmol~leu~l^{-1}~h^{-1}))
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Save --------------------------------------------------------------------

ggsave("graphs/fig10.pdf",
  device = cairo_pdf,
  width = 7,
  height = 5 / 2
)
