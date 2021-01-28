# Figure on primary production

rm(list = ls())

source("R/interpolate_fun.R")

pp <- data.table::fread(
  "data/raw/database 17nov2010__DATA.csv",
  skip = 3,
  select = c(1, 2, 5, 20),
  col.names = c(
    "cast",
    "bottle",
    "depth",
    "primary_production_mgc_m3_24h"
  )
) %>%
  as_tibble() %>%
  drop_na(primary_production_mgc_m3_24h) %>%
  filter(depth <= 100)

pp

stations <- read_csv("data/clean/stations.csv") %>%
  distinct(station, cast, transect, .keep_all = TRUE) %>%
  filter(station != 345)

df <- inner_join(stations, pp, by = "cast") %>%
  filter(transect %in% c(600, 300)) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

df %>%
  count(station, depth, sort = TRUE)

df %>%
  ggplot(aes(x = primary_production_mgc_m3_24h)) +
  geom_histogram() +
  facet_wrap(~transect, scales = "free")

# Remove the extreme value of PP (544) as discussed with Patrick Rimbault (see
# email).

df <- df %>%
  filter(!primary_production_mgc_m3_24h == max(primary_production_mgc_m3_24h))

df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x") +
  ggrepel::geom_text_repel(aes(label = station))

# Average by station, depth

df <- df %>%
  # select(-date) %>%
  group_by(station, transect, depth) %>%
  summarise(across(everything(), mean), n = n()) %>%
  ungroup()

df %>%
  ggplot(aes(x = latitude, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x") +
  ggrepel::geom_text_repel(aes(label = station))

# Interpolation -----------------------------------------------------------

res <- df %>%
  # filter(primary_production_mgc_m3_24h <= 25) %>%
  group_nest(transect) %>%
  mutate(interpolated_pp = map(
    data,
    interpolate_2d,
    x = latitude,
    y = depth,
    z = primary_production_mgc_m3_24h
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

p <- res %>%
  unnest(interpolated_pp) %>%
  select(-data) %>%
  drop_na(z) %>%
  mutate(z = ifelse(z < 0, 0, z)) %>%
  ggplot(aes(
    x = x,
    y = y,
    z = z,
    fill = z
  )) +
  geom_isobands(color = NA, breaks = seq(0, 25, by = 0.25)) +
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
    trans = "log10",
    breaks = c(1, 10, 25),
    limits = c(1, 25),
    labels = c("0", "10", ">25"),
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
    fill = bquote(Primary~production~(mgC~m^{-3}~d^{-1}))
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

# Save --------------------------------------------------------------------

ggsave("graphs/fig13.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 7,
  units = "cm"
)

# Test --------------------------------------------------------------------

# Maybe add histograms of nitrification?

df <- read_csv("data/raw/csv/novembre.csv")

stations <- read_csv("data/clean/stations.csv") %>%
  select(station, cast, transect) %>%
  distinct(cast, .keep_all = TRUE)

df <- left_join(df, stations)

p2 <- df %>%
  drop_na(nitrification_2464_nmoles_n_l_1_24h_1) %>%
  ggplot(aes(x = nitrification_2464_nmoles_n_l_1_24h_1)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_log10(breaks = scales::breaks_log()) +
  annotation_logticks(sides = "b", size = 0.25) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
  )

p3 <- df %>%
  drop_na(reg_nh4_2466_nmoles_n_l_1_24h_1) %>%
  ggplot(aes(x = reg_nh4_2466_nmoles_n_l_1_24h_1)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_log10(breaks = scales::breaks_log()) +
  annotation_logticks(sides = "b", size = 0.25) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
  )

p / (p2 + p3) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))
