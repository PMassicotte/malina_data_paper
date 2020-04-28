rm(list = ls())

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

# Plot --------------------------------------------------------------------

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

p <- cdom %>%
  filter(wav == 254) %>%
  inner_join(doc) %>%
  mutate(suva254 = absorption / doc_mg) %>%
  ggplot(aes(x = latitude, y = suva254)) +
  geom_line() +
  geom_point() +
  facet_wrap(~transect, scales = "free_x", ncol = 1) +
  ggrepel::geom_text_repel(aes(label = station),
  size = 2.5,
  color = "gray50",
  box.padding = unit(0.15, "lines")
) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = "Latitude",
    y = bquote(SUVA[254]~(L~m^{-1}~mgC^{-1}))
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  "graphs/fig06.pdf",
  device = cairo_pdf,
  width = 5,
  height = 5
)
