table1 <- read_sheet("1KOnp0XAyTZYUUx7wL7TMNBP3yo4utWW--jZQsnbcMNk", sheet = "table1") %>%
  janitor::clean_names() %>%
  rename(
    Parameter = parameter,
    Method = method,
    `Sampling device` = sampling,
    `Principal investigators` = pi
  )

table1 <- table1 %>%
  mutate(across(where(is.character), ~ str_replace_all(., "#", "\\\\#"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\(Lu\\(z\\)\\)", "($L_u(z)$)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\(Eu\\(z\\)\\)", "($E_u(z)$)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\(Ed\\(z\\)\\)", "($E_d(z)$)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Ed\\(0\\+\\)", "$E_d(0^+)$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "NH4", "NH$^+_4$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "NO3", "NO$^-_3$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "NO2", "NO$^-_2$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\(0\\+\\)", "($0^+$)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Si\\(OH\\)4", "$Si(OH)_4$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "PO4", "(PO$_4)^{3-}$")))

table1 %>%
  kable(
    "latex",
    longtable = TRUE,
    booktabs = TRUE,
    caption = "Parameters measured during the MALINA oceanographic expedition. Parameters are ordered by alphabetical order.",
    escape = FALSE
  ) %>%
  kable_styling(
    latex_options = c("repeat_header"),
    font_size = 6
  ) %>%
  # row_spec(c(0), bold = TRUE) %>%
  landscape() %>%
  write_lines(here::here("tables/table1.tex"))
