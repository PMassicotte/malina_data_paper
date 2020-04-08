rm(list = ls())

df <- read_csv("data/raw/csv/shiptrack.csv") %>%
  select(-starts_with("x")) %>%
  mutate(longitude = -longitude) %>%
  mutate(month = case_when(
    str_detect(date, "juil") ~ 7,
    str_detect(date, "aout") ~ 8,
    TRUE ~ NA_real_
  )) %>%
  mutate(date = parse_number(date)) %>%
  mutate(date = as.Date(paste("2009", month, date), format = "%Y %m %d")) %>%
  select(-month) %>%
  verify(longitude < 0)

df %>%
  arrange(date) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_path()

write_csv(df, "data/clean/shiptrack.csv")
