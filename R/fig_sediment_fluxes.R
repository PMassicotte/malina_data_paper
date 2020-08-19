
df <- read_excel("data/raw/Link_et_al_Malina_for_Massicotte.xlsx",
  skip = 49
) %>%
  janitor::clean_names() %>%
  mutate(transect = station %/% 100 * 100, .after = station)

df %>%
  count(depth_m, samp_type)

stations <- read_csv("data/clean/stations.csv") %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station:longitude)

df <- df %>%
  left_join(stations, by = "station") %>%
  relocate(latitude, longitude, .after = station) %>%
  filter(samp_type == "Sediment") %>%
  type_convert()

df

# id_incubation_core is the replicate number
df %>%
  count(id_incubation_core, sort = TRUE)

df <- df %>%
  select(
    station,
    latitude,
    longitude,
    transect,
    id_incubation_core,
    contains("mmol")
  ) %>%
  group_by(station, latitude, longitude, transect) %>%
  summarise(across(contains("mmol"),
    list(mean = mean, sd = sd),
    na.rm = TRUE
  ), n = n()) %>%
  mutate(transect = factor(transect, levels = c(600, 300, 200, 100)))

# Identify north and south stations
df <- df %>%
  group_by(transect) %>%
  mutate(
    lat_position = ifelse(station == max(station), "South", "North"),
    .after = transect
  ) %>%
  mutate(
    station_label = glue::glue("{station} ({lat_position})"),
    .after = lat_position
  ) %>%
  mutate(station_label = as.character(station_label)) %>%
  mutate(station_label = fct_reorder(station_label, lat_position,
    .desc = FALSE
  ))

facet_lab <- c(
  "100" = "Transect 100",
  "200" = "Transect 200",
  "300" = "Transect 300",
  "600" = "Transect 600"
)

p <- df %>%
  ggplot(aes(
    x = station_label,
    y = tou_mmol_m_2_day_net_of_sediment_mean
  )) +
  geom_col() +
  facet_wrap(~transect, scales = "free_x", labeller = labeller(transect = facet_lab)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 1)) +
  scale_y_reverse() +
  labs(
    x = NULL,
    y = bquote("Oxygen uptake" ~ (mmol ~ m^{-2} ~ day^{-1})),
    fill = "ID\n(incubation core)"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing.y = unit(2, "lines")
  )

ggsave(
  "graphs/fig_o2_uptake.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 14,
  units = "cm"
)


# df %>%
#   # filter(transect %in% c(600, 300)) %>%
#   pivot_longer(contains("mmol")) %>%
#   filter(str_detect(name, "mean")) %>%
#   ggplot(aes(
#     x = factor(station),
#     y = value
#   )) +
#   geom_col() +
#   facet_wrap(transect~name, scales = "free") +
#   # scale_y_reverse() +
#   labs(
#     x = NULL,
#     fill = "ID\n(incubation core)"
#   )
