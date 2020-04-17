rm(list = ls())

cdom <-
  fread(here::here("data/raw/", "malina_leg2b_acdom_allspec_for_Simon.csv")) %>%
  as_tibble() %>%
  pivot_longer(-wav, names_to = "filename", values_to = "absorption")

cdom %>%
  count(filename, sort = TRUE) %>%
  assertr::verify(n == 536)

# How many samples do we have?
cdom %>%
  distinct(filename)

cdom %>%
  filter(between(wav, 254, 600)) %>%
  ggplot(aes(x = wav, y = absorption, group = filename)) +
  geom_line(size = 0.1)

# Extract stations --------------------------------------------------------

# Extract station information from the file names

cdom_clean <- cdom %>%
  extract(
    filename,
    into = c("station_cast", "type", "depth", "niskin_no"),
    regex = "(\\d{3})([a-z])(\\d{3})(.*)",
    remove = FALSE,
    convert = TRUE
  ) %>%
  # filter(wav == 200) %>%
  mutate(niskin_no = ifelse(type == "n", parse_number(niskin_no), NA))

# Most of the CDOM samples are from niskin bottles
cdom_clean %>%
  count(type, sort = TRUE)

cdom_clean %>%
  filter(wav <= 500) %>%
  ggplot(aes(x = wav, y = absorption, group = filename)) +
  geom_line(size = 0.25) +
  facet_wrap(~type)

# After discussion with Flavienne, they are not sure what are the "b" type. I
# will flush it.
cdom_clean %>%
  filter(type == "b") %>%
  distinct(filename)

cdom_clean <- cdom_clean %>%
  filter(type != "b")

cdom_clean %>%
  filter(wav <= 500) %>%
  ggplot(aes(x = wav, y = absorption, group = filename)) +
  geom_line(size = 0.25) +
  facet_wrap(~type)

# Save data ---------------------------------------------------------------

write_csv(cdom_clean, "data/clean/cdom_ultrapath.csv")

