rm(list = ls())

# Discharge data
# https://wateroffice.ec.gc.ca/search/historical_e.html
# https://wateroffice.ec.gc.ca/report/historical_e.html?stn=10LC014&dataType=Daily&parameterType=Flow&year=2009&mode=Table&y1Max=1&y1Min=1

water_flow <- fread("data/raw/Daily__Sep-6-2019_05_08_04PM.csv") %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::ymd(date))

station <- read_csv("data/clean/stations.csv") %>%
  left_join(water_flow, by = "date") %>%
  filter(
    lubridate::year(date) == 2009,
    lubridate::month(date) == 8,
    param == 1
  ) %>%
  distinct(date, station, .keep_all = TRUE)

water_flow_2009 <- water_flow %>%
  filter(
    lubridate::year(date) == 2009,
    lubridate::month(date) == 8,
    param == 1
  )

p <- water_flow_2009 %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_point(data = station, aes(
    x = date,
    y = value,
    color = factor(transect)
  )) +
  scale_x_date(breaks = scales::breaks_pretty(n = 4), expand = expansion(mult = c(0.1, 0.1))) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 4), labels = scales::label_number_si()) +
  xlab(NULL) +
  ylab(bquote("Daily discharge" ~ (m^3 ~ s^{-1}))) +
  paletteer::scale_colour_paletteer_d("ggsci::default_nejm") +
  labs(
    color = "Transect",
    title = "Discharge of the river",
    subtitle = "Station 10LC014: MACKENZIE RIVER AT ARCTIC RED RIVER"
  ) +
  theme(
    plot.caption = element_text(size = 4),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "none"
  ) +
  facet_wrap(~transect, ncol = 2)

ggsave(
  "graphs/fig02.pdf",
  device = cairo_pdf,
  width = 12,
  height = 12,
  units = "cm"
)
