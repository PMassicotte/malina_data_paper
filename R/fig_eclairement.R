rm(list = ls())

df <- read_csv("data/raw/csv/eclairement.csv") %>%
  pivot_longer(cols = starts_with("eclairement"), names_to = "wavelength", values_to = "ed_atm_u_wcm_2_nm_1") %>%
  mutate(wavelength = str_match(wavelength, "\\d{3}")) %>%
  mutate(wavelength = parse_number(wavelength)) %>%
  mutate(date = lubridate::parse_date_time(date, orders = "dmy")) %>%
  mutate(date_time = date + time)

df %>%
  # filter(wavelength == 412) %>%
  ggplot(aes(x = date_time, y = ed_atm_u_wcm_2_nm_1)) +
  geom_line() +
  facet_wrap(~wavelength, scales = "free")
