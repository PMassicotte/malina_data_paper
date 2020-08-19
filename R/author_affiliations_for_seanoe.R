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
    institution = "Takuvik Joint International Laboratory / UMI 3376, ULAVAL (Canada) - CNRS (France), Université Laval, Québec, QC, Canada",
    orcid = "0000-0002-5919-4116",
    .before = 1
  ) %>%
  add_row(
    firstname = "Marcel",
    lastname = "Babin",
    email = "marcel.babin@takuvik.ulaval.ca",
    institution = "Takuvik Joint International Laboratory / UMI 3376, ULAVAL (Canada) - CNRS (France), Université Laval, Québec, QC, Canada"
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


# First format ------------------------------------------------------------

authors_v1 <- authors %>%
  group_nest(lastname, firstname, email, orcid) %>%
  mutate(data = map(data, ~mutate(., id = paste0("institution", 1:nrow(.))))) %>%
  unnest(cols = data) %>%
  pivot_wider(names_from = id, values_from = "institution") %>%
  rename_all(~str_to_title(.)) %>%
  rename(
    Nom = Lastname,
    `Prénom` = Firstname
  )

authors_v1_pm <- filter(authors_v1, Nom == "Massicotte" & `Prénom` == "Philippe")
authors_v1_mb <- filter(authors_v1, Nom == "Babin" & `Prénom` == "Marcel")

authors_v1_others <- authors_v1 %>%
  filter(!str_detect(Nom, "Massicotte|Babin"))

bind_rows(authors_v1_pm, authors_v1_others, authors_v1_mb) %>%
  write_csv("data/clean/affiliations_seanoe_v1.csv", na = "")

# Second format -----------------------------------------------------------

setDT(authors)[, id := .GRP, by = institution]

authors <- authors %>%
  as_tibble() %>%
  mutate(affiliation = paste0("affiliation", id)) %>%
  select(-id) %>%
  pivot_wider(names_from = affiliation, values_from = institution)

authors %>%
  rename_all(~str_to_title(.)) %>%
  rename(
    Nom = Lastname,
    `Prénom` = Firstname
  ) %>%
  write_csv("data/clean/affiliations_seanoe_v2.csv", na = "")
