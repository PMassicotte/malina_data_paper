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
    legend.margin = margin(r = 2)
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

p2 <- df %>%
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
    guide = guide_legend(ncol = 3)
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
    legend.margin = margin(r = 4)
  )

# Save --------------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(nrow = 2, heights = c(0.2, 0.55)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave("graphs/fig09.pdf",
  device = cairo_pdf,
  width = 10,
  height = 18,
  units = "cm"
)
