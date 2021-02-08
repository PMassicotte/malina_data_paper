table1 <- read_sheet("1KOnp0XAyTZYUUx7wL7TMNBP3yo4utWW--jZQsnbcMNk", sheet = "table1") %>%
  janitor::clean_names() %>%
  rename(
    Parameters = parameter,
    Method = method,
    Sampling = sampling,
    `Principal investigators` = pi
  ) %>%
  janitor::remove_empty("cols")

table1

df <- table1 %>%
  pivot_longer(matches("reference\\d+"),
    names_to = "reference",
    values_to = "doi"
  )

dois <- df %>%
  drop_na(doi) %>%
  distinct(doi, .keep_all = FALSE)

dois %>%
  mutate(bibtex_citation_key = "") %>%
  write_csv("~/Desktop/dois.csv")
