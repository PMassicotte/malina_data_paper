# Figure of bacterial production. Note that there are also data for bacterial
# respiration, but only like 10 observations. I will not make a figure for BR.

rm(list = ls())

source("R/interpolate_fun.R")

# Read all the data -------------------------------------------------------

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
  mutate(transect = factor(transect, levels = c("600", "300")))

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

p <-
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
    fill = bquote("Bacterial production"~(mu*gC~m^{-3}~d^{-1}))
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

ggsave("graphs/fig12.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 7,
  units = "cm"
)
