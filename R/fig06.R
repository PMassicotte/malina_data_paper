rm(list = ls())


# Dissolved absorption ----------------------------------------------------

df <- read_csv("data/clean/cdom_ultrapath.csv") %>%
  mutate(transect = station_cast %/% 100 * 100) %>%
  filter(transect %in% c(300, 600)) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

# There are mostly surface CDOM
df %>%
  filter(wav == 200) %>%
  count(depth)

df <- df %>%
  filter(depth == 0)

# Type:
# n = niskin
# z = zodiacc
# g = barge
df %>%
  filter(wav <= 500) %>%
  ggplot(aes(x = wav, y = absorption, group = filename)) +
  geom_line(size = 0.25) +
  facet_wrap(~type)

# Looks like are some spectra were duplicated, i.e. measured at the same
# location but form the zodiac and the "barge".
dup <- df %>%
  filter(wav == 200) %>%
  count(station_cast, depth, sort = TRUE) %>%
  filter(n > 1) %>%
  pull(station_cast)

dup

df %>%
  filter(station_cast %in% dup) %>%
  ggplot(aes(x = wav, y = absorption, group = filename)) +
  geom_line() +
  facet_wrap(~station_cast, scales = "free")

# Average CDOM absorption spectra

cdom <- df %>%
  group_by(wav, station_cast, depth, transect) %>%
  summarise(absorption = mean(absorption, na.rm = TRUE), n = n()) %>%
  rename(station = station_cast) %>%
  ungroup()

cdom %>%
  ggplot(aes(x = wav, y = absorption, group = station)) +
  geom_line() +
  facet_wrap(~transect)

# Get station information

stations <- read_csv("data/clean/stations.csv") %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, longitude, latitude)

cdom %>%
  anti_join(stations, by = "station") %>%
  filter(wav == 200)

cdom <- cdom %>%
  inner_join(stations, by = "station")

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

# Plot aCDOM --------------------------------------------------------------

df_viz <- cdom %>%
  filter(between(wav, 254, 600)) %>%
  filter(station %in% c(697, 620, 398, 320)) %>%
  group_by(transect) %>%
  mutate(position = ifelse(
    station == max(station),
    "Estuary stations (south)",
    "Open water stations (north)"
  )) %>%
  ungroup() %>%
  mutate(position = factor(
    position,
    levels = c("Estuary stations (south)", "Open water stations (north)")
  ))

df_station <- df_viz %>%
  group_by(station) %>%
  filter(wav == min(wav)) %>%
  ungroup()

p1 <- df_viz %>%
  ggplot(aes(x = wav, y = absorption, group = station)) +
  geom_line() +
  facet_wrap(~position, ncol = 1, scales = "free_y") +
  geom_text(
    data = df_station,
    aes(label = station),
    size = 2.5,
    hjust = 1.25,
    color =
      "gray50"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.12, 0.05)),
    breaks = seq(250, 600, by = 50)
  ) +
  # coord_cartesian(xlim = c(250, 600), expand = TRUE) +
  scale_y_continuous(
    expand = expansion(mult = c(0.01, 0.12)),
    breaks = scales::breaks_pretty(n = 5)
  ) +
  labs(
    x = "Wavelength (nm)",
    y = bquote(italic(a)["CDOM"]~(m^{-1}))
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# How many times higher the absorption is higher between north and south
# stations? Approximately 15 times higher.
df_viz %>%
  filter(wav == 254) %>%
  group_by(transect) %>%
  summarise(difference = max(absorption) / min(absorption))

# Plot SUVA ---------------------------------------------------------------

set.seed(1234)

# SUVA254

doc <- read_csv("data/raw/csv/doc.csv") %>%
  select(station, depth, doc_u_mc) %>%
  group_by(station, depth) %>%
  summarise(across(everything(), mean)) %>%
  ungroup() %>%
  mutate(doc_mc = doc_u_mc / 1e6) %>%
  mutate(doc_g = doc_mc * 12) %>%
  mutate(doc_mg = doc_g * 1000)

doc

p2 <- cdom %>%
  filter(wav == 254) %>%
  inner_join(doc) %>%
  mutate(suva254 = absorption / doc_mg) %>%
  ggplot(aes(x = latitude, y = suva254)) +
  geom_line() +
  geom_point() +
  facet_wrap(~transect, scales = "free_x", ncol = 2, labeller = labeller(transect = lab)) +
  ggrepel::geom_text_repel(aes(label = station),
  size = 2.5,
  color = "gray50",
  box.padding = unit(0.25, "lines")
) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) +
  labs(
    x = "Latitude",
    y = bquote(SUVA[254]~(L~m^{-1}~mgC^{-1}))
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Particulate absorption --------------------------------------------------

ap <- read_csv("data/raw/csv/aptot.csv") %>%
  janitor::clean_names() %>%
  filter(pressure == 0) %>%
  filter(method == "BRG") %>%
  select(-ap443_1)

ap

df <- ap %>%
  pivot_longer(starts_with("ap"), names_to = "wavelength", values_to = "ap") %>%
  mutate(wavelength = parse_number(wavelength)) %>%
  filter(between(wavelength, 254, 600)) %>%
  filter(station %in% c(697, 620, 398, 320)) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100)

df

df <- df %>%
  group_by(station, wavelength, method, transect) %>%
  summarise(ap = mean(ap, na.rm = TRUE), n = n()) %>%
  group_by(transect) %>%
  mutate(position = ifelse(
    station == max(station),
    "Estuary stations (south)",
    "Open water stations (north)"
  )) %>%
  ungroup() %>%
  mutate(position = factor(
    position,
    levels = c("Estuary stations (south)", "Open water stations (north)")
  ))

# Compare ap(443) for estuary and open water stations
df %>%
  filter(wavelength == 443) %>%
  ggplot(aes(x = factor(station), y = ap, fill = position)) +
  geom_col() +
  geom_text(aes(label = round(ap, digits = 2)), vjust = -1)

df %>%
  ggplot(aes(x = wavelength, y = ap, color = factor(station))) +
  geom_line() +
  facet_wrap(~transect, scales = "free")

df_station <- df %>%
  group_by(station) %>%
  filter(wavelength == min(wavelength)) %>%
  ungroup()

p3 <- df %>%
  ggplot(aes(x = wavelength, y = ap, group = station)) +
  geom_line() +
  facet_wrap(~position, scales = "free", ncol = 1) +
  geom_text(
    data = df_station,
    aes(label = station),
    size = 2.5,
    hjust = 1.25,
    color =
      "gray50"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.12, 0.05)),
    breaks = seq(250, 800, by = 50)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.01, 0.12)),
    breaks = scales::breaks_pretty(n = 5)
  ) +
  labs(
    x = "Wavelength (nm)",
    y = bquote(italic(a)[p]~(m^{-1}))
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- {p1 + p3} / p2 +
  plot_layout(heights = c(0.5, 0.25)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  "graphs/fig06.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 15,
  units = "cm"
)
