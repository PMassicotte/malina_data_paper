# Eclairement -------------------------------------------------------------

rm(list = ls())

eu <- read_csv("data/raw/csv/eclairement.csv") %>%
  pivot_longer(starts_with("eclairement"), names_to = "wavelength", values_to = "eu_w_cm_2_nm_1") %>%
  mutate(date = lubridate::dmy(date)) %>%
  extract(wavelength, into = c(NULL, "wavelength"), regex = "_(\\d+)nm$", convert = TRUE) %>%
  mutate(id = group_indices(., date, time))

eu %>%
  count(id, sort = TRUE) %>%
  verify(n == 8)

eu %>%
  ggplot(aes(x = wavelength, y = eu_w_cm_2_nm_1, group = id, color = lubridate::hour(time))) +
  geom_line(size = 0.25, alpha = 0.2) +
  facet_wrap(~date) +
  scale_color_viridis_c() +
  labs(
    color = "Hour of\nthe day"
  )

eu %>%
  filter(wavelength == 412) %>%
  ggplot(aes(x = time, y = eu_w_cm_2_nm_1)) +
  geom_point() +
  facet_wrap(~date)

eu
