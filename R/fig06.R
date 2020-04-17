rm(list = ls())

source("R/interpolate_fun.R")

hplc <- readxl::read_excel(
  "data/raw/malina-pig-20120918.xls",
  sheet = "data"
) %>%
  janitor::clean_names()

df <- hplc %>%
  mutate(across(is.character, .fns = ~ str_remove(., "LOD"))) %>%
  type_convert() %>%
  select(
    date = sampling_date_utc,
    latitude = latitude_n,
    longitude = longitude_e,
    station,
    ctd,
    depth_m,
    bottle,
    total_chlorophyll_a
  ) %>%
  mutate(date = as.Date(date))

df

df %>%
  count(station, sort = TRUE)

df %>%
  count(ctd, sort = TRUE)

# Keep only CTD observations
df <- df %>%
  filter(str_detect(station, "^\\d{3}$")) %>%
  type_convert()

df <- df %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(transect %in% c(300, 600)) %>%
  mutate(transect = factor(transect, levels = c("600", "300"))) %>%
  filter(depth_m <= 100)

# Should we have only 1 measure per station, per depth?
df %>%
  count(station, depth_m, sort = TRUE)

# Plot of the 300 and 600 transect

df %>%
  distinct(station, transect, longitude, latitude, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude, color = factor(transect))) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = station), show.legend = FALSE) +
  labs(
    title = "HPLC stations for transects 300 and 600",
    subtitle = "Wrong coordinates for many stations...Should not have anything bellow 69N",
    color = "Transect"
  )

ggsave(
  "graphs/hplc_problem_geographical coordinates.png",
  type = "cairo",
  dpi = 600
)

df %>%
  distinct(station, transect, longitude, latitude, .keep_all = TRUE) %>%
  filter(between(latitude, 65, 72)) %>%
  filter(station != 345) %>%
  ggplot(aes(x = longitude, y = latitude, color = factor(transect))) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = station), show.legend = FALSE) +
  labs(
    title = "HPLC stations for transects 300 and 600",
    subtitle = "Wrong coordinates for many stations...Should not have anything bellow 69N",
    color = "Transect"
  )

# Keep only stations in the straight line transects
df <- df %>%
  filter(between(latitude, 65, 72)) %>%
  filter(station != 345)

df %>%
  ggplot(aes(x = latitude, y = depth_m)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

# Interpolation -----------------------------------------------------------

res <- df %>%
  group_nest(transect) %>%
  mutate(interpolated_chla = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth_m,
    z = total_chlorophyll_a
  ))

res

res2 <- res %>%
  unnest(interpolated_chla) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  drop_na(z)

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  # filter(depth_m == min(depth_m)) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE)

station_labels

p <- res2 %>%
  ggplot(aes(x = x, y = y, z = z, fill = z)) +
  geom_isobands(color = NA, breaks = seq(0, 200, by = 0.1)) +
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
    aes(x = latitude, y = depth_m),
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
    trans = "sqrt",
    guide =
      guide_colorbar(
        barwidth = unit(0.5, "cm"),
        barheight = unit(4, "cm")
      )
  ) +
  labs(
    x = "Latitude",
    y = "Depth (m)",
    fill = "TChla"
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank()
  )

ggsave("graphs/fig06.pdf",
  device = cairo_pdf,
  width = 7,
  height = 5 / 2
)
