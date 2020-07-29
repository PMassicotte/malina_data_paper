# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Create a LaTeX pastable list of authors. To be used within the ESSD LaTeX
# template.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Read the Google sheet ---------------------------------------------------

authors <- read_sheet("1KOnp0XAyTZYUUx7wL7TMNBP3yo4utWW--jZQsnbcMNk") %>%
  janitor::clean_names() %>%
  arrange(lastname)

# Add myself and Marcel as first and last authors -------------------------

authors <- authors %>%
  filter(!str_detect(lastname, "Massicotte|Babin")) %>%
  add_row(
    firstname = "Philippe",
    lastname = "Massicotte",
    email = "philippe.massicotte@takuvik.ulaval.ca",
    institution = "UMI Takuvik, CNRS/Université Laval, Québec, QC Canada",
    orcid = "0000-0002-5919-4116",
    .before = 1
  ) %>%
  add_row(
    firstname = "Marcel",
    lastname = "Babin",
    email = "marcel.babin@takuvik.ulaval.ca",
    institution = "UMI Takuvik, CNRS/Université Laval, Québec, QC Canada"
  )

# Extract the institution -------------------------------------------------

extract_institution <- function(institution) {

  institution %>%
    str_split("\\(\\d*\\)") %>%
    map(function(x){x[!x ==""]}) %>%
    map(str_trim) %>%
    unlist()
}

authors <- authors %>%
  replace_na(list(institution = "unidentified")) %>%
  mutate(institution = map(institution, extract_institution)) %>%
  unnest(cols = institution)

authors <- authors %>%
  select(lastname, firstname, email, orcid, institution)

setDT(authors)[, id := .GRP, by = institution]

authors %>%
  as_tibble() %>%
  mutate(affiliation = paste0("affiliation", id)) %>%
  select(-id) %>%
  pivot_wider(names_from = affiliation, values_from = institution) %>%
  write_csv("~/Desktop/affiliations.csv")
