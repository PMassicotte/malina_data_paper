rm(list = ls())

source("R/interpolate_fun.R")

files <- fs::dir_ls(
  "data/raw/ctd_jens/AC9_barge/AC9_barge/",
  glob = "*.txt"
)

# a and bb have been measured by two AC9, here I compare how similar/different
# are a440 measured by these two device.

# df <- vroom::vroom(files, id =
# "filename") %>% janitor::clean_names() %>% select(filename, pres_dbar,
# starts_with("a_440"))
#
# df %>% pivot_longer(starts_with("a_440")) %>% ggplot(aes(x = value, y =
# pres_dbar, color = name)) + geom_path() + scale_y_reverse() +
# facet_wrap(~basename(filename), scales = "free")

df <- vroom::vroom(files,
  id = "filename",
  .name_repair = "minimal"
) %>%
  janitor::clean_names() %>%
  drop_na(pres_dbar) %>%
  select(
    filename,
    pres_dbar,
    starts_with("a_"),
    starts_with("b_"),
    starts_with("c_"),
    starts_with("bbp_")
  )

df

# Tidy the data -----------------------------------------------------------

df <- df %>%
  pivot_longer(matches("^a_|^b_|^c_|^bbp_")) %>%
  separate(
    name,
    into = c("variable", "wavelength", "device"),
    fill = "right",
    convert = TRUE
  ) %>%
  mutate(device = replace_na(device, "1")) %>%
  # filter(device == "1") %>%
  pivot_wider(names_from = variable)

df

# Extract the cast from the filename --------------------------------------

df <- df %>%
  mutate(cast = str_match(filename, "archive_(\\d+)_")[, 2]) %>%
  mutate(cast = parse_number(cast))

# Get station numbers -----------------------------------------------------

cast <- c(136L, 137L, 138L, 139L, 140L, 141L, 155L, 158L, 159L, 160L, 161L, 162L, 163L, 164L, 166L, 168L, 169L, 171L, 172L, 174L, 175L, 176L, 177L, 178L, 179L, 180L, 181L, 182L, 183L, 185L, 186L, 187L, 188L, 189L, 190L, 191L, 192L, 193L, 194L, 195L, 196L, 197L, 198L, 199L, 200L, 201L, 202L, 203L, 204L, 205L, 206L, 207L, 208L, 209L, 210L, 211L, 212L, 213L, 214L)

station <- c("691", "691", "692", "680", "394", "394", "280", "280", "260", "260", "220", "220", "240", "240", "240", "170", "170", "150", "150", "380", "380", "360", "360", "320", "320", "340", "340", "670", "670", "660", "660", "620", "620", "760", "760", "780", "695", "694", "697", "696", "691", "345_1", "345_1", "345_2", "398", "397", "396", "395", "540", "540", "430", "430", "460", "135_1", "135_2", "135_2", "235", "235_2", "235_2")

cast_station <- tibble(cast, station)

df %>%
  anti_join(cast_station, by = "cast")

df <- df %>%
  left_join(cast_station, by = "cast") %>%
  filter(str_detect(station, "^\\d{3}$")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100)

df

# Get geographical coordinates
coordinates <- read_csv("data/clean/stations.csv") %>%
  select(-transect, -cast) %>%
  distinct(station, .keep_all = TRUE)

# One station not matching...
df %>%
  anti_join(coordinates) %>%
  distinct(station)

df <- df %>%
  inner_join(coordinates, )


# Calculate bb ------------------------------------------------------------

# Use pure water bbw to compute bb (reminder: bb = bbp + bbw)

bbw_hydrolight <- readxl::read_excel("data/raw/HydroLight_PureWater_IOPs.xls") %>%
  janitor::clean_names() %>%
  select(
    wavelength = iop_number,
    bbw_hydrolight = b_bw
  )

bbw_equation <- tibble(
  wavelength = seq(350, 800, by = 1),
  bbw_morel = 0.0023 * (450 / wavelength) ^ 4.32,
  bbw_zhang = 0.0020 * (450 / wavelength) ^ 4.3
)

bbw_equation %>%
  left_join(bbw_hydrolight) %>%
  pivot_longer(starts_with("bbw")) %>%
  drop_na() %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line()

df <- df %>%
  mutate(bbw_morel = 0.0023 * (450 / wavelength) ^ 4.32) %>%
  mutate(bb = bbp + bbw_morel)


# Average replicates ------------------------------------------------------

# I will round the depth to the closest meter

# df <- df %>%
#   mutate(depth = round(pres_dbar, digits = 1), .after = pres_dbar) %>%
#   group_by(depth, wavelength, device, station, transect) %>%
#   summarise(across(a:bbp, ~ mean(., na.rm = TRUE)), n = n())

# Explore -----------------------------------------------------------------

df %>%
  filter(transect %in% c(300, 600)) %>%
  filter(wavelength == 440) %>%
  drop_na(a) %>%
  ggplot(aes(
    x = a,
    y = pres_dbar,
    color = device,
    group = interaction(filename, device)
  )) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~ glue::glue("{station}\n{basename(filename)}"), scales = "free")

# Some stations have many vertical profiles. I will just choose the first one as
# it does not make sens to average replicates because they do not share the same
# depths.

df_viz <- df %>%
  filter(transect %in% c(300, 600)) %>%
  filter(wavelength == 440) %>%
  group_nest(station, filename) %>%
  distinct(station, .keep_all = TRUE) %>%
  unnest(data) %>%
  mutate(transect = factor(transect, levels = c(600, 300)))

# Keep vertical profiles with at least 5 measurements
df_viz_a <- df_viz %>%
  drop_na(a) %>%
  add_count(station) %>%
  filter(n >= 5)

df_viz_a %>%
  ggplot(aes(
    x = a,
    y = pres_dbar,
    color = device,
    group = interaction(filename, device)
  )) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~ glue::glue("{station}\n{basename(filename)}"), scales = "free")


# Interpolation for a -----------------------------------------------------

df_viz_a <- df_viz %>%
  drop_na(a) %>%
  add_count(station) %>%
  filter(n >= 5)

res <- df_viz_a %>%
  group_nest(transect) %>%
  mutate(interpolated_a = map(
    data,
    interpolate_2d,
    x = latitude,
    y = pres_dbar,
    z = a,
    n = 1,
    m = 1,
    h = 4
  ))

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

facet_lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p1 <- res %>%
  select(-data) %>%
  unnest(interpolated_a) %>%
  drop_na(z) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  ggplot(aes(x = x, y = y, z = z, fill = z)) +
  geom_raster() +
  geom_isobands(color = NA, breaks = seq(0, 5, by = 0.1)) +
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
    aes(x = latitude, y = pres_dbar),
    size = 0.05,
    color = "gray50",
    inherit.aes = FALSE
  ) +
  facet_wrap(~transect, scales = "free_x", labeller = labeller(transect = facet_lab)) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.15))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c("oompaBase::jetColors",
    trans = "sqrt",
    breaks = scales::pretty_breaks(n = 6),
    # breaks = seq(0, 120, by = 20),
    # limits = c(0, 1.6),
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
    fill = bquote(italic(a)(440) ~ "["*m^{-1}*"]")
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(hjust = 0, size = 10, face = "bold"),
    strip.text.y = element_text(hjust = 0.5, size = 6),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )


# Interpolation fore bb ---------------------------------------------------

df_viz_bb <- df_viz %>%
  drop_na(bb) %>%
  add_count(station) %>%
  filter(n >= 5)

res <- df_viz_bb %>%
  group_nest(transect) %>%
  mutate(interpolated_bb = map(
    data,
    interpolate_2d,
    x = latitude,
    y = pres_dbar,
    z = bb,
    n = 1,
    m = 1,
    h = 4
  ))

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

facet_lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p2 <- res %>%
  select(-data) %>%
  unnest(interpolated_bb) %>%
  drop_na(z) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  ggplot(aes(x = x, y = y, z = z, fill = z)) +
  geom_raster() +
  geom_isobands(color = NA, breaks = seq(0, 5, by = 0.005)) +
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
    aes(x = latitude, y = pres_dbar),
    size = 0.05,
    color = "gray50",
    inherit.aes = FALSE
  ) +
  facet_wrap(~transect, scales = "free_x", labeller = labeller(transect = facet_lab)) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.15))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c("oompaBase::jetColors",
    trans = "sqrt",
    breaks = scales::pretty_breaks(n = 8),
    # limits = c(0, 0.5),
    oob = scales::squish,
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
    fill = bquote(italic(b[b])(440) ~ "["*m^{-1}*"]")
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(hjust = 0, size = 10, face = "bold"),
    strip.text.y = element_text(hjust = 0.5, size = 6),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

# Save plot ---------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave("graphs/fig05.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 14,
  units = "cm"
)
