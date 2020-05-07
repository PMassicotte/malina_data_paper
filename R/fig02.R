rm(list = ls())


# Water flow --------------------------------------------------------------

# Discharge data
# https://wateroffice.ec.gc.ca/search/historical_e.html
# https://wateroffice.ec.gc.ca/report/historical_e.html?stn=10LC014&dataType=Daily&parameterType=Flow&year=2009&mode=Table&y1Max=1&y1Min=1

water_flow <- fread("data/raw/Daily__Sep-6-2019_05_08_04PM.csv") %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(param == 1) %>%
  mutate(date = lubridate::ymd(date))

range(water_flow$date)

water_flow_1972_2016 <- water_flow %>%
  group_by(yday = lubridate::yday(date)) %>%
  filter(yday <= 365) %>%
  summarise(mean_value = mean(value)) %>%
  mutate(date = as.Date(glue("2009-{yday}"), "%Y-%j"))

water_flow_2009 <- water_flow %>%
  filter(lubridate::year(date) == 2009)

# Extract water discharge during the Malina cruise
shiptrack <- read_csv("data/clean/shiptrack.csv")
range(shiptrack$date)
water_flow_malina <- water_flow %>%
  filter(between(date, min(shiptrack$date), max(shiptrack$date)))

# lab <- water_flow_malina %>%
#   filter(date == min(date) | date == max(date))

p1 <- water_flow_2009 %>%
  ggplot(aes(x = date, y = value)) +
  geom_area(
    data = water_flow_1972_2016,
    aes(x = date, y = mean_value),
    fill = "#3c3c3c",
    alpha = 0.25
  ) +
  geom_line(size = 0.5, color = "black") +
  geom_line(
    data = water_flow_malina,
    color = "#CA3E47",
    size = 0.75,
    lineend = "round"
  ) +
  scale_x_date(
    expand = expansion(mult = c(0.1, 0.1)),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = scales::label_number_si()
  ) +
  xlab(NULL) +
  ylab(bquote("Daily discharge" ~ (m^3 ~ s^{
    -1
  }))) +
  paletteer::scale_colour_paletteer_d("ggsci::default_nejm") +
  theme(
    legend.key.size = unit(0.5, "cm"),
    legend.position = "none",
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


# Air temperature ---------------------------------------------------------

air_temperature <- vroom::vroom("data/raw/csv/procmet.csv") %>%
  mutate(date = lubridate::make_datetime(year, month, day, hour, min, sec)) %>%
  filter(between(date, "2009-07-30", "2009-08-25")) %>%
  drop_na(t_hmp_avg)

air_temperature

# Hourly average
air_temperature_average <- air_temperature %>%
  mutate(date_hour = lubridate::floor_date(date, unit = "hour")) %>%
  group_by(date_hour) %>%
  summarise(mean_t_hmp_avg = mean(t_hmp_avg, na.rm = TRUE))


p2 <- air_temperature_average %>%
  ggplot(aes(x = date_hour, y = mean_t_hmp_avg)) +
  geom_line(size = 0.3, color = "#3c3c3c") +
  scale_x_datetime(
    date_breaks = "3 days",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  geom_hline(yintercept = 0, color = "red", lty = 2, size = 0.25) +
  labs(
    x = NULL,
    y = "Air temperature (°C)"
  ) +
  theme(
    legend.key.size = unit(0.5, "cm"),
    legend.position = "none",
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  "graphs/fig02.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 17.5 / 1.25,
  units = "cm"
)

# Stats for the paper -----------------------------------------------------

range(water_flow_1972_2016$mean_value)
range(water_flow_malina$value)

# When is the maximum discharge?
water_flow_1972_2016 %>%
  filter(mean_value == max(mean_value))
