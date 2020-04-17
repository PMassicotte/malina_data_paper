rm(list = ls())

df <- read_csv("data/raw/csv/hookerpigment.csv")

glimpse(df)

# What is the difference between chl_a and t_chl_a?
df %>%
  ggplot(aes(x = concentration_chl_a, y = concentration_t_chl_a)) +
  geom_point() +
  geom_abline(color = "red", lty = 2)

df %>%
  count(station, depth, sort = TRUE)

# Looks like there is only surface data
df %>%
  count(depth)

df %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(aes(size = concentration_t_chl_a)) +
  ggrepel::geom_text_repel(aes(label = station))

# Some stations are duplicated... What are the differences?
df %>%
  filter(str_detect(station, "680"))

# Graph with transects 300 and 600
df_viz <- df %>%
  separate(
    station,
    into = c("station", "replicate"),
    sep = "-",
    fill = "right",
    convert = TRUE
  ) %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(transect %in% c(300, 600))

df_viz

df_viz %>%
  ggplot(aes(x = factor(latitude), y = concentration_t_chl_a)) +
  geom_col() +
  facet_wrap(~transect, scales = "free_x")
