library(rvest)

# https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/

url <- "https://wateroffice.ec.gc.ca/search/historical_results_e.html?search_type=basin&basin=10&start_year=1850&end_year=2020&minimum_years=&gross_drainage_operator=%3E&gross_drainage_area=&effective_drainage_operator=%3E&effective_drainage_area="

hydrometric_stations <- read_html(url) %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(str_detect(data_availability, "Flow"))

hydrometric_stations

flow <- vroom::vroom("data/raw/river_flow/DLY_FLOWS.csv")

flow <- flow %>%
  filter(station_number %in% hydrometric_stations$station_number)

flow %>%
  count(station_number, year)

flow <- flow %>%
  select(station_number, year, month, matches("^flow\\d+")) %>%
  pivot_longer(matches("^flow\\d+"), names_to = "day", values_to = "flow") %>%
  drop_na(flow) %>%
  mutate(day = parse_number(day)) %>%
  mutate(date = lubridate::make_date(year, month, day))

flow

flow %>%
  filter(station_number == "10LC014") %>%
  # filter(year >= 2009) %>%
  ggplot(aes(x = date, y = flow, group = station_number)) +
  geom_line()

flow %>%
  group_by(station_number) %>%
  summarise(mean_flow = mean(flow, na.rm = TRUE)) %>%
  mutate(station_number = fct_reorder(station_number, mean_flow)) %>%
  top_n(20, mean_flow) %>%
  ggplot(aes(x = mean_flow, y = station_number)) +
  geom_col()
