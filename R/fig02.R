# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure to show sea ice concentration during the mission and the
# 200 meters isobath.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Geotiff
# https://wvs.earthdata.nasa.gov/?LAYERS=MODIS_Terra_CorrectedReflectance_TrueColor&CRS=EPSG:4326&TIME=2009-08-21&COORDINATES=68.000000,-141.000000,72.500000,-125.000000&FORMAT=image/tiff&AUTOSCALE=TRUE&RESOLUTION=250m

# Isobath 200 m
# http://www.naturalearthdata.com/downloads/10m-physical-vectors/


# Stations ----------------------------------------------------------------

station <- read_csv("data/clean/stations.csv") %>%
  distinct(station, .keep_all = TRUE) %>%
  dplyr::select(station, longitude, latitude, transect)

# bbox used to crop
bb <- st_bbox(c(
  xmin = -150,
  xmax = -120,
  ymin = 68,
  ymax = 75
), crs = 4326)

bb


# MODIS true color --------------------------------------------------------

r <- read_stars(here::here("data/raw/ice_cover/snapshot-2009-08-21.tiff")) %>%
  st_crop(bb)

# 200 meters isobath ------------------------------------------------------

isobath_200m <- st_read(here::here("data/raw/ice_cover/ne_10m_bathymetry_K_200/ne_10m_bathymetry_K_200.shp"))

# Land --------------------------------------------------------------------

ne_land <-
  rnaturalearth::ne_download(
    category = "physical",
    type = "land",
    returnclass = "sf",
    scale = "large"
  )

# MODIS true color plot ---------------------------------------------------

p1 <-
  ggplot() +
  ggspatial::layer_spatial(data = r) +
  geom_sf(data = isobath_200m, fill = NA, color = "red") +
  geom_point(data = station, aes(
    x = longitude,
    y = latitude,
    color = factor(transect)
  ), show.legend = FALSE) +
  paletteer::scale_color_paletteer_d("ggsci::default_nejm") +
  coord_sf(xlim = c(-141, -125), ylim = c(68.5, 72.5)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~"2009-08-21") +
  theme(
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# Sea ice concentration ---------------------------------------------------

# ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G10033/north/weekly/shapefile/

files <- fs::dir_ls("data/raw/ice_cover/sic/", recurse = TRUE, glob = "*.shp")

sic <- map(files, st_read) %>%
  set_names(files) %>%
  do.call(what = sf:::rbind.sf, .) %>%
  rownames_to_column(var = "date") %>%
  mutate(date = str_match(date, "\\d{8}")) %>%
  mutate(date = as.Date(date, format = "%Y%m%d"))

sic

bb_stereo <- isobath_200m %>%
  st_crop(bb) %>%
  st_transform(crs = st_crs(sic))

sic <- sic %>%
  st_crop(bb_stereo) %>%
  st_transform(crs = 4326)

sic

sic %>%
  count(tc_mid)

# Plot --------------------------------------------------------------------

p2 <- sic %>%
  ggplot() +
  geom_sf(data = ne_land, size = 0.15, fill = "gray75") +
  geom_sf(aes(fill = tc_mid / 100), color = NA) +
  geom_sf(data = isobath_200m, fill = NA, color = "red") +
  geom_point(data = station, aes(
    x = longitude,
    y = latitude,
    color = factor(transect)
  ),
  show.legend = FALSE,
  size = 0.5) +
  paletteer::scale_color_paletteer_d("ggsci::default_nejm") +
  scale_fill_gradient(
    low = "#063F6F",
    high = "white",
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = scales::label_percent(),
    guide = guide_legend(
      label.position = "top",
      title.position = "top",
      title = "Sea ice concentration",
      title.theme = element_text(hjust = 0.5),
      keywidth = unit(1, "cm"),
      keyheight = unit(0.25, "cm"),
      override.aes = list(color = "#3c3c3c", size = 0.25)
    )
  ) +
  coord_sf(xlim = c(-141, -125), ylim = c(68.5, 72.5)) +
  facet_wrap(~date) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#063F6F"),
    legend.position = "bottom",
    legend.margin = margin(t = -30),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs/fig02.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 10
)

knitr::plot_crop(here::here("graphs/fig02.pdf"))
