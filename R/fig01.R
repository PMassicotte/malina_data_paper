# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Plot of the stations positions.
# Bathymetry data: https://download.gebco.net/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(raster)

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

# Prepare bathymetry data -------------------------------------------------

bathy <- raster::raster("data/raw/bathymetry/GEBCO_2019_27_Apr_2020_5e98c581281a/gebco_2019_n75.0_s68.0_w-145.0_e-120.0.tif") %>%
  # raster::sampleRegular(size = 1e4, asRaster = TRUE) %>%
  raster::rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3) %>%
  filter(between(y, 69, 73))

bathy_interpolated <- bathy %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z))

range(bathy_interpolated$xyz.est.z)

# Plot --------------------------------------------------------------------

set.seed(123)

# Some stations have many locations, check if they are at least grouped in the
# same area. After visual inspection, I can confirm that the same station name
# but with different coordinates are very close to each other.

# station <- station %>% add_count(station, sort = TRUE) %>% filter(n > 1)

p <- ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 20, color = NA
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-4000, 0),
    oob = scales::squish,
    guide = guide_legend(
      label.position = "top",
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8),
      label.theme = element_text(size = 6),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(1, "cm")
    ),
    breaks = -c(0, 1000, 2000, 3000)
  ) +
  geom_sf(data = ne_land, size = 0.15) +
  geom_sf(data = river_network, size = 0.01, color = "gray75") +
  coord_sf(xlim = c(-141, -125), ylim = c(68.5, 72.5)) +
  geom_point(data = station, aes(
    x = longitude,
    y = latitude,
    color = factor(transect)
  ), show.legend = FALSE) +
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
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.25,
    height = unit(0.1, "cm")
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.direction = "horizontal",
    legend.position = c(0.99, 0.1),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "#B9DDF1")
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

detach("package:raster", unload = TRUE)
