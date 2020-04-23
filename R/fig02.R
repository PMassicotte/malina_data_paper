rm(list = ls())

# Discharge data
# https://wateroffice.ec.gc.ca/search/historical_e.html
# https://wateroffice.ec.gc.ca/report/historical_e.html?stn=10LC014&dataType=Daily&parameterType=Flow&year=2009&mode=Table&y1Max=1&y1Min=1

water_flow <- fread("data/raw/Daily__Sep-6-2019_05_08_04PM.csv") %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(param == 1) %>%
  mutate(date = lubridate::ymd(date))

water_flow_2009 <- water_flow %>%
  filter(lubridate::year(date) == 2009)

# Extract water discharge during the Malina cruise
shiptrack <- read_csv("data/clean/shiptrack.csv")
range(shiptrack$date)
water_flow_malina <- water_flow %>%
  filter(between(date, min(shiptrack$date), max(shiptrack$date)))

# lab <- water_flow_malina %>%
#   filter(date == min(date) | date == max(date))

p <- water_flow_2009 %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(size = 0.5, color = "gray50") +
  geom_line(
    data = water_flow_malina,
    color = "#CA3E47",
    size = 0.75,
    lineend = "round"
  ) +
  scale_x_date(
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = scales::label_number_si()
  ) +
  xlab(NULL) +
  ylab(bquote("Daily discharge" ~ (m^3 ~ s^{-1}))) +
  paletteer::scale_colour_paletteer_d("ggsci::default_nejm") +
  theme(
    plot.caption = element_text(size = 4),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave(
  "graphs/fig02.pdf",
  device = cairo_pdf,
  width = 7,
  height = 3
)
