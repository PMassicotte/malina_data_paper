# Figure on bacterial abundance

rm(list = ls())

source("R/interpolate_fun.R")

df <- read_csv("data/raw/csv/cytometry.csv") %>%
  mutate(date = as.Date(date, "%d-%b-%y")) %>%
  select(station, cast = ctd, depth, bacteria_cell_m_l) %>%
  filter(depth <= 100) %>%
  filter(station != 345) %>%
  drop_na(bacteria_cell_m_l)

df

# Get data for which we have a station number

df <- df %>%
  filter(str_detect(station, "^\\d{3}$")) %>%
  type_convert()

df

# Get station information

stations <- read_csv("data/clean/stations.csv") %>%
  distinct(station, cast, .keep_all = TRUE)

anti_join(df, stations)

cyto <- inner_join(df, stations) %>%
  filter(transect %in% c(300, 600))

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

# Interpolation -----------------------------------------------------------

res <- cyto %>%
  group_nest(transect) %>%
  mutate(interpolated_bacteria_cell_m_l = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = bacteria_cell_m_l,
    n = 1,
    m = 1,
    h = 5
  ))

res

# Plot --------------------------------------------------------------------

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

p <- res %>%
  unnest(interpolated_bacteria_cell_m_l) %>%
  select(-data) %>%
  drop_na(z) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(0, 2e8, by = 3e4)) +
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
  facet_wrap(~transect, scales = "free_x") +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.1))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    breaks = scales::breaks_pretty(n = 5),
    labels = scales::label_number_si(accuracy = 0.1),
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
    fill = bquote(Bacteria~(cells~mL^{-1}))
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

# Save --------------------------------------------------------------------

ggsave("graphs/fig08.pdf",
  device = cairo_pdf,
  width = 7,
  height = 3
)
