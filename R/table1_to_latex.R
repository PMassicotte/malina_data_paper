# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create Table 1.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

table1 <- read_sheet("1KOnp0XAyTZYUUx7wL7TMNBP3yo4utWW--jZQsnbcMNk", sheet = "table1") %>%
  janitor::clean_names() %>%
  rename(
    Parameters = parameter,
    Method = method,
    Sampling = sampling,
    `Principal investigators` = pi,
    `Included in the data repository` = included_in_data_files
  ) %>%
  distinct() %>%
  arrange(Parameters)

# Add references ----------------------------------------------------------

table1

bibtex_citation_keys <- read_csv2(here::here("tables/dois.csv"))

bibtex_df <- read_csv(here::here("tables/table1_bibtex_references.csv")) %>%
  janitor::clean_names() %>%
  mutate(
    bibtex_citation_key = str_match(extra, regex("Citation Key: (\\S+)"))[, 2],
    .after = "author"
  ) %>%
  select(
    author,
    bibtex_citation_key,
    title,
    publication_year,
    doi,
    publication_title
  )

bibtex_df

# Keep only the first author for each publication
bibtex_df <- bibtex_df %>%
  separate_rows(author, sep = ";") %>%
  mutate(author = str_squish(author)) %>%
  add_count(bibtex_citation_key) %>%
  group_by(bibtex_citation_key) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(author = ifelse(n > 1, glue("{author} et al."), author)) %>%
  select(-n)

references <- table1 %>%
  pivot_longer(starts_with("reference"), values_to = "doi") %>%
  left_join(bibtex_citation_keys, by = "doi") %>%
  select(-doi, -name) %>%
  drop_na(bibtex_citation_key) %>%
  nest_join(bibtex_df, by = "bibtex_citation_key") %>%
  # slice(1:20) %>%
  mutate(citation_text = map(bibtex_df, function(bibtex_df) {
    if (is.na(bibtex_df$doi)) {
      glue_data(
        bibtex_df,
        "{author} ({publication_year}) {publication_title}"
      )} else {
        glue_data(
          bibtex_df,
          "{author} ({publication_year}) {publication_title} [{doi}]"
        )
      }
    }
  )) %>%
  unnest(citation_text) %>%
  select(-c(bibtex_df))

references

references %>%
  distinct(citation_text)

references %>%
  janitor::get_dupes(citation_text) %>%
  distinct(citation_text)

references <- references %>%
  distinct(citation_text) %>%
  mutate(Reference = row_number()) %>%
  inner_join(references) %>%
  relocate(c(citation_text, Reference), .after = `Principal investigators`)

references %>%
  distinct(Reference)

# references <- references %>%
#   group_by(citation_text) %>%
#   mutate(Reference = cur_group_id(), .after = `Principal investigators`) %>%
#   ungroup()

references

table1 <- table1 %>%
  select(-starts_with("reference")) %>%
  left_join(
    references %>% select(-c(citation_text, `Included in the data repository`)),
    by = c("Parameters", "Method", "Sampling", "Principal investigators")
  )

table1 <- table1 %>%
  group_nest(
    Parameters,
    Method,
    Sampling,
    `Principal investigators`,
    `Included in the data repository`,
    bibtex_citation_key
  ) %>%
  mutate(Reference = map_chr(data, ~ paste(.$Reference, collapse = ", "))) %>%
  select(-data)

# Format the table --------------------------------------------------------

table1_formatted <- table1 %>%
  mutate(across(where(is.character), ~ str_replace_all(., "#", "\\\\#"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\(Lu\\(z\\)\\)", "($L_u(z)$)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\(Eu\\(z\\)\\)", "($E_u(z)$)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\(Ed\\(z\\)\\)", "($E_d(z)$)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Ed\\(0\\+\\)", "$E_d(0^+)$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "NH4", "NH$^+_4$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "NO3", "NO$^-_3$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "NO2", "NO$^-_2$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\(0\\+\\)", "($0^+$)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Si\\(OH\\)4", "Si(OH)\\\\textsubscript{4}"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "PO4", "(PO$_4)^{3-}$"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "O2", "O\\\\textsuperscript{2}"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "CO3", "CO\\\\textsuperscript{3}"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\)\\(", "\\) \\("))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "(\\w+)\\(", "\\1 \\("))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "\\)(\\w+)", "(\\w+) \\1"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "14C", "\\\\textsuperscript{14}C"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "13C", "\\\\textsuperscript{13}C"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "15C", "\\\\textsuperscript{15}C"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "18O", "\\\\textsuperscript{18}O"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "15N", "\\\\textsuperscript{15}N"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "N2", "N\\\\textsubscript{2}"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "210Pb", "\\\\textsuperscript{210}Pb"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "226Ra", "\\\\textsuperscript{226}Ra"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "228Ra", "\\\\textsuperscript{228}Ra"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "234Th", "\\\\textsuperscript{234}Th"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "238U", "\\\\textsuperscript{238}U"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "137Cs", "\\\\textsuperscript{137}Cs"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "209Po", "\\\\textsuperscript{209}Po"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "H2O", "H\\\\textsubscript{2}O"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Leucine-3H|Leu-3H", "Leucine-\\\\textsuperscript{3}H"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Bacterial abundance", "Bacterial \\(abundance\\)"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "(\\d{3})nm", "\\1 nm"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "P waves", "P-waves"))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Particle Size Ditribution", "Particle Size Distribution")))

# Prepare the footnote ----------------------------------------------------

# I changed the footnote as required by the ESSD type writer. The footnote now
# includes the bibtex citation keys.

# fn <- references %>%
#   distinct(Reference, citation_text, bibtex_citation_key) %>%
#   summarise(fn = glue("({Reference}) {citation_text}")) %>%
#   pull(fn) %>%
#   paste0(collapse = "; ")

fn <- references %>%
  distinct(Reference, citation_text, bibtex_citation_key) %>%
  summarise(fn = glue("\\citet{{{bibtex_citation_key}}}")) %>%
  pull(fn) %>%
  paste0(collapse = "; ")

fn

# Save the table ----------------------------------------------------------

table1_formatted %>%
  mutate(Reference = ifelse(Reference == "NA", "", Reference)) %>%
  kable(
    "latex",
    longtable = TRUE,
    booktabs = TRUE,
    caption = "Parameters measured during the MALINA oceanographic expedition. Parameters are ordered alphabetically.",
    escape = FALSE
  ) %>%
  kable_styling(
    latex_options = c("repeat_header"),
    font_size = 4
  ) %>%
  footnote(
    general = fn,
    threeparttable = TRUE,
    fixed_small_size = FALSE,
    escape = TRUE,
    general_title = ""
  ) %>%
  # row_spec(c(0), bold = TRUE) %>%
  # landscape() %>%
  write_lines(here::here("tables/table1_with_citet_commands.tex"))


