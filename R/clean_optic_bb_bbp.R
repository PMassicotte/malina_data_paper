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

df %>%
  count(spectra_id, sort = TRUE) %>%
  verify(n == 8)

df %>%
  filter(bbp < 1) %>%
  ggplot(aes(x = wavelength, y = bb, group = spectra_id)) +
  geom_line()

df %>%
  filter(station != 42) %>%
  # filter(bbp < 1) %>%
  drop_na(bbp) %>%
  # filter(station %in% c(2, 10, 11)) %>%
  ggplot(aes(x = bbp, y = depth, color = factor(wavelength))) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~station, scales = "free")

df %>%
  filter(station != 42) %>%
  write_csv("data/clean/bb_bbp.csv")
