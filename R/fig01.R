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

# station <- read_excel("data/raw/ctd_jens/malina_stations_all.xls", sheet = 2) %>%
#   janitor::clean_names() %>%
#   dplyr::select(-time_utc) %>%
#   mutate(station = parse_number(station)) %>%
#   arrange(station) %>%
#   mutate(date = as.Date(date, "%m/%d/%y")) %>%
#   distinct(station, .keep_all = TRUE) %>%
#   mutate(transect = station %/% 100 * 100) %>%
#   dplyr::select(station, longitude, latitude, transect)

station <- read_csv("data/clean/stations.csv") %>%
  distinct(station, .keep_all = TRUE) %>%
  dplyr::select(station, longitude, latitude, transect)

# station <- station %>%
#   bind_rows(station2)

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

bathy <- raster::raster(
  "data/raw/bathymetry/GEBCO_2019_27_Apr_2020_5e98c581281a/gebco_2019_n75.0_s68.0_w-145.0_e-120.0.tif"
) %>%
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

p1 <- ggplot() +
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
      keywidth = unit(0.75, "cm"),
      byrow = TRUE,
      nrow = 1
    ),
    breaks = -seq(0, 3000, by = 500)
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
    family = "Poppins",
    size = 3
  ) +
  annotate(
    "text",
    y = 70.2,
    x = -126,
    label = "Amundsen\nGulf",
    family = "Poppins",
    size = 3
  ) +
  annotate(
    "text",
    y = 72.2,
    x = -137.5,
    label = "Beaufort Sea",
    family = "Poppins",
    size = 6,
    fontface = "plain"
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

# Bathymetry profiles -----------------------------------------------------

# Extract bathymetry along the south -> north gradient

transect <- station %>%
  filter(transect %in% c(600, 300)) %>%
  filter(station != 345) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  cbind(st_coordinates(.)) %>%
  group_by(transect) %>%
  arrange(Y) %>%
  summarise(m = mean(station), do_union = FALSE) %>%
  st_cast("LINESTRING")

transect %>%
  ggplot() +
  geom_sf()

r <- raster::raster(
  "data/raw/bathymetry/GEBCO_2019_27_Apr_2020_5e98c581281a/gebco_2019_n75.0_s68.0_w-145.0_e-120.0.tif"
)

transect_bathy <- raster::extract(r, transect, along = TRUE, cellnumbers = TRUE)
transect_bathy_df <- map_dfr(transect_bathy, as_tibble, .id = "ID") %>%
  janitor::clean_names() %>%
  rename("depth" = 3)

transect_bathy_coords <- coordinates(r)[transect_bathy_df$cell, ] %>%
  as_tibble() %>%
  set_names(c("longitude", "latitude"))

transect_bathy_df <- transect_bathy_df %>%
  cbind(transect_bathy_coords) %>%
  as_tibble()

transect_bathy_df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_path() +
  facet_wrap(~id, scales = "free_x")

# Extract bathymetry at station locations

station_coordinates <- station %>%
  filter(transect %in% c(600, 300)) %>%
  filter(station != 345) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

station_bathy <- raster::extract(r, station_coordinates, along = TRUE, cellnumbers = TRUE) %>%
  as_tibble() %>%
  rename("depth" = 2)

station_bathy <- station_bathy %>%
  cbind(station_coordinates) %>%
  st_as_sf() %>%
  cbind(st_coordinates(.)) %>%
  rename(longitude = X, latitude = Y)

station_bathy %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  facet_wrap(~transect)

# Final bathymetry plot ---------------------------------------------------

transect_bathy_df <- transect_bathy_df %>%
  mutate(transect = ifelse(id == 1, 300, 600)) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

station_bathy <- station_bathy %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

paletteer::paletteer_d("ggsci::default_nejm")

p2 <- transect_bathy_df %>%
  ggplot(aes(x = latitude, y = depth, color = transect)) +
  geom_path() +
  geom_point(data = station_bathy) +
  ggrepel::geom_text_repel(
    data = station_bathy,
    aes(label = station),
    size = 2,
    segment.size = 0.25,
    box.padding = unit(0.25, "lines"),
    nudge_y = -100
  ) +
  scale_color_manual(
    values = c(
      "600" = "#6F99ADFF",
      "300" = "#E18727FF"
    )
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = "Latitude",
    y = "Depth (m)"
  ) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank()
  )

p <- p1 + p2 +
  plot_layout(ncol = 1, heights = c(0.75, 0.25)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

destfile <- "graphs/fig01.pdf"

ggsave(
  destfile,
  device = cairo_pdf,
  width = 17.5,
  height = 17.5,
  units = "cm"
)

knitr::plot_crop(destfile)

# Stats for the paper -----------------------------------------------------

# Depth at all stations

r <- raster::raster(
  "data/raw/bathymetry/GEBCO_2019_27_Apr_2020_5e98c581281a/gebco_2019_n75.0_s68.0_w-145.0_e-120.0.tif"
)

station_coordinates <- station %>%
  # filter(transect %in% c(600, 300)) %>%
  filter(station != 345) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

station_bathy <- raster::extract(
  r,
  station_coordinates,
  along = TRUE,
  cellnumbers = TRUE
) %>%
  as_tibble() %>%
  rename(depth = 2)

station_bathy %>%
  ggplot(aes(x = depth)) +
  geom_histogram(binwidth = 25)

range(station_bathy$depth)
mean(station_bathy$depth)
sd(station_bathy$depth)

detach("package:raster", unload = TRUE)
