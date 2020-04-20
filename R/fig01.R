# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Plot of the stations positions.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

station <- read_csv("data/clean/stations.csv") %>%
  distinct(station, .keep_all = TRUE)

ne_land <-
  rnaturalearth::ne_download(
    category = "physical",
    type = "land",
    returnclass = "sf",
    scale = "large"
  )

ne_river <-
  rnaturalearth::ne_download(
    category = "physical",
    type = "rivers_lake_centerlines",
    returnclass = "sf",
    scale = "large"
  )

ship_track <- read_csv("data/clean/shiptrack.csv")

arrow <- tibble(
  x = -137,
  xend = -136.17,
  y = 68.6,
  yend = 68.94
)

river_coords <- tibble(lon = c(-136.172778), lat = c(68.939722))

# River network -----------------------------------------------------------

# https://open.canada.ca/data/en/dataset/448ec403-6635-456b-8ced-d3ac24143add
river_network <-
  st_read("data/raw/lakes_rivers_shapefiles/ghy_000c11a_e/ghy_000c11a_e.shp") %>%
  st_crop(c(
    xmin = -141,
    xmax = -125,
    ymin = 68,
    ymax = 72.5
  ))

# Plot --------------------------------------------------------------------

set.seed(123)

# Some stations have many locations, check if they are at least grouped in the
# same area. After visual inspection, I can confirm that the same station name
# but with different coordinates are very close to each other.

# station <- station %>% add_count(station, sort = TRUE) %>% filter(n > 1)

p <- ggplot() +
  geom_sf(data = ne_land, size = 0.15) +
  geom_sf(data = river_network, size = 0.01, color = "gray75") +
  geom_path(
    data = ship_track,
    aes(x = longitude, y = latitude),
    color = "gray75",
    size = 0.25
  ) +
  coord_sf(xlim = c(-141, -125), ylim = c(68.5, 72.5)) +
  geom_point(data = station, aes(
    x = longitude,
    y = latitude,
    color = factor(transect)
  )) +
  geom_curve(
    data = arrow,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = -0.3,
    size = 0.25,
    arrow = arrow(length = unit(0.05, "inch"))
  ) +
  ggrepel::geom_text_repel(
    data = station,
    aes(x = longitude, y = latitude, label = station),
    size = 2.5,
    segment.size = 0.25
  ) +
  annotate(
    "text",
    y = 68.5,
    x = -137,
    label = "Mackenzie Delta",
    family = "Exo"
  ) +
  annotate(
    "text",
    y = 70.2,
    x = -126,
    label = "Amundsen\nGulf",
    family = "Exo"
  ) +
  annotate(
    "text",
    y = 72.2,
    x = -137.5,
    label = "Beaufort Sea",
    family = "Exo",
    size = 6,
    fontface = 2
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    height = unit(0.1, "cm")
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid.major = element_line(
      color = gray(0.75),
      linetype = "dashed",
      size = 0.25
    ),
    legend.position = "none"
  ) +
  paletteer::scale_color_paletteer_d("ggsci::default_nejm")

# p + facet_wrap(~date)
# p + facet_wrap(~station)

destfile <- "graphs/fig01.pdf"

ggsave(
  destfile,
  device = cairo_pdf,
  width = 7,
  height = 6
)

knitr::plot_crop(destfile)


# ggsave(
#   "graphs/fig01.png",
#   type = "cairo",
#   dpi = 600,
#   width = 7,
#   height = 6
# )
