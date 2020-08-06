rm(list = ls())

df <- data.table::fread("data/raw/csv/bbsiop.csv") %>%
  mutate_if(is.character, ~str_replace(., ",", "\\.")) %>%
  filter(str_detect(sal, "sal", negate = TRUE)) %>%
  type_convert() %>%
  as_tibble() %>%
  pivot_longer(matches("^bb"), names_to = "wavelength", values_to = "value") %>%
  extract(wavelength, into = c("var", "wavelength"), regex = "(bb|bbp)(\\d+)", convert = TRUE) %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(spectra_id = group_indices(., fileme, depth))

df <- df %>%
  mutate(station = parse_number(station_alias)) %>%
  mutate(transect = station %/% 100 * 100)

df %>%
  count(spectra_id, sort = TRUE) %>%
  verify(n == 8)

df %>%
  filter(bbp < 1) %>%
  ggplot(aes(x = wavelength, y = bb, group = spectra_id)) +
  geom_line()

df %>%
  filter(transect %in% c(300, 600)) %>%
  drop_na(bb) %>%
  ggplot(aes(x = bb, y = depth, color = factor(wavelength))) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~station, scales = "free")

df %>%
  filter(station != 42) %>%
  write_csv("data/clean/bb_bbp.csv")
