# Figure on bacteria

rm(list = ls())

source("R/interpolate_fun.R")


# Cytometry (abundance) ---------------------------------------------------

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
  filter(transect %in% c(600))

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

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p1 <- res %>%
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
  facet_wrap(~transect, scales = "free_x", labeller = labeller(transect = lab)) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.15))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    breaks = scales::breaks_width(width = 3e5),
    labels = scales::label_scientific(),
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
    fill = bquote("Bacteria"~("cells"~"mL"^{-1}))
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(r = 2),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

# Bacterial production ----------------------------------------------------

file <- "data/raw/BacterialProduction_Malina_30151_2479.xls"

df1 <- read_excel(file, sheet = 2) %>%
  janitor::clean_names() %>%
  select(
    latitude = lat,
    longitude = lon,
    station,
    depth,
    bp_pmol_leu_l_1_h_1
  ) %>%
  filter(station != 394) # This station is included in the zodiac data

df2 <- read_excel(file, sheet = 4) %>%
  janitor::clean_names() %>%
  select(
    latitude = lat,
    longitude = lon,
    station,
    bp_pmol_leu_l_1_h_1
  ) %>%
  mutate(depth = 0)

df <- bind_rows(df1, df2)

# Merge coordinates -------------------------------------------------------

df <- df %>%
  group_by(station) %>%
  mutate(across(c(latitude, longitude), mean)) %>%
  ungroup()

# Convert pmol to ug ------------------------------------------------------

# x = pmol leu l-1 h-1
# 1e-12 = convertir en pmole en mole
# 1.5 = kg C par mole
# 1e9 = kg vers ug
# 1e3 = l-1 en m-3
# 24 = pour passer de h-1 en d-1

df <- df %>%
  mutate(bp_ug_c_m_3_d_1 = bp_pmol_leu_l_1_h_1 * 1e-12 * 1.5 * 1e9 * 1e3 * 24)

df %>%
  ggplot(aes(x = bp_ug_c_m_3_d_1)) +
  geom_histogram(binwidth = 100)

# Filter out the data for the graph ---------------------------------------

df <- df %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(transect %in% c(300, 600)) %>%
  filter(depth <= 100) %>%
  mutate(transect = factor(transect, levels = c("600", "300"))) %>%
  filter(transect == "600")

df

df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  facet_wrap(~transect, scales = "free_x") +
  scale_y_reverse() +
  ggrepel::geom_text_repel(aes(label = station))

# Interpolation -----------------------------------------------------------

res <- df %>%
  group_nest(transect) %>%
  mutate(interpolated_bp = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = bp_ug_c_m_3_d_1,
    n = 1,
    m = 1,
    h = 5
  ))

# Plot --------------------------------------------------------------------

station_labels <- res %>%
  unnest(data) %>%
  group_by(transect) %>%
  ungroup() %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, transect, latitude)

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

p2 <-
  res %>%
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
  geom_isobands(color = NA, breaks = seq(0, 10000, by = 200)) +
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
  facet_wrap(~transect, scales = "free_x", labeller = labeller(transect = lab)) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.15))) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    # trans = "sqrt",
    breaks = scales::breaks_pretty(n = 8),
    labels = scales::label_number(),
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
    fill = bquote("Bacterial production"~("µgC m"^{-3}~d^{-1}))
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

# Bacterial diversity -----------------------------------------------------

col_names <-
  read_excel(
    "data/raw/new_data/Fig454_Malina.xlsx",
    .name_repair = "minimal",
    n_max = 1
  ) %>%
  paste(., names(.)) %>%
  str_squish() %>%
  str_to_lower()

df <- read_excel(
  "data/raw/new_data/Fig454_Malina.xlsx",
  skip = 2,
  col_names = col_names,
  n_max = 12
) %>%
  pivot_longer(-class, names_to = "tmp", values_to = "relative_contribution") %>%
  mutate(relative_contribution = relative_contribution / 100) %>%
  extract(
    tmp,
    into = c("location", "type", "station"),
    regex = "(\\S+)\\_(\\S+)\\W+(\\d{3})",
    remove = FALSE,
    convert = TRUE
  )

df

lab <- c(
  "fl" = "Free-living bacteria",
  "pa" = "Particle-attached bacteria"
)

p3 <- df %>%
  mutate(class = fct_relevel(class, "Other", after = Inf)) %>%
  mutate(location = str_to_title(location)) %>%
  mutate(location2 = glue("{location} ({station})")) %>%
  mutate(location2 = factor(location2, levels = c("River (697)", "Coast (694)", "Sea (620)"))) %>%
  ggplot(aes(x = location2, y = relative_contribution, fill = class)) +
  geom_col() +
  facet_wrap(~type, labeller = labeller(type = lab), ncol = 1) +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  paletteer::scale_fill_paletteer_d(
    "ggsci::default_igv",
    guide = guide_legend(ncol = 2)
  ) +
  labs(
    x = NULL,
    y = "Relative abundance"
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(3, "mm"),
    legend.position = "bottom",
    legend.margin = margin(t = -25)
  )

# Save --------------------------------------------------------------------

p <- ((p1 / p2) | p3) +
  plot_layout(widths = c(0.45, 0.35)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

# +
#   plot_layout(ncol = 1, heights = c(0.25, 0.25, 0.5), byrow = FALSE) +
#   plot_annotation(tag_levels = "A") &
#   theme(plot.tag = element_text(face = "bold"))

ggsave(
  "graphs/fig14.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 14,
  units = "cm"
)
