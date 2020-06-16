rm(list = ls())

url <- "http://www.obs-vlfr.fr/proof/php/malina/x_datalist_1.php?xxop=malina&xxcamp=malina"

df <- read_html(url) %>%
  html_nodes("fieldset") %>%
  html_nodes("table") %>%
  html_table(header = TRUE) %>%
  bind_rows() %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  select(
    parameter = no_param,
    method,
    sampling,
    pi = resp
  )

df <- df %>%
  mutate(parameter = str_remove(parameter, "\\(\\d+\\)\\s+"))

df %>%
  write_csv("data/clean/table1.csv")

