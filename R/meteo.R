
df <- read_csv("data/raw/csv/procmet.csv", na = "NA", guess_max = 1e5) %>%
  mutate(date_time = lubridate::make_datetime(year, month, day, hour, min, sec))

  p <- df %>%
  drop_na(t_hmp_avg) %>%
  ggplot(aes(x = date_time, y = t_hmp_avg)) +
  geom_line() +
  facet_wrap(~lubridate::as_date(date_time), scales = "free_x", ncol = 4) +
  scale_x_datetime(breaks = scales::pretty_breaks(n = 6), date_labels = "%H")

ggsave(
  "graphs/meteo.pdf",
  device = cairo_pdf,
  width = 8, height = 10
)
