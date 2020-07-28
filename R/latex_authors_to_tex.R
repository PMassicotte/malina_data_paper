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

institutions <- authors %>%
  select(institution) %>%
  distinct(institution, .keep_all = TRUE) %>%
  rowid_to_column(var = "institution_id")

df <- authors %>%
  left_join(institutions) %>%
  group_by(firstname, lastname) %>%
  nest() %>%
  mutate(institution_id = map_chr(data, ~paste(.$institution_id, collapse = ",")))

# Create a list of authors ------------------------------------------------

latex_author <- df %>%
  mutate(latex_author = paste0("\\", glue::glue("Author[{institution_id}]{{{firstname}}}{{{lastname}}}"))) %>%
  pull(latex_author)

# Create a list of institutions -------------------------------------------

latex_institution <- df %>%
  select(-institution_id) %>%
  unnest(data) %>%
  mutate(latex_institution = paste0("\\", glue::glue("affil[{institution_id}]{{{institution}}}"))) %>%
  mutate(latex_institution = str_replace(latex_institution, "&", "\\\\&")) %>%
  pull(latex_institution) %>%
  unique()

# Copy to clipboard -------------------------------------------------------

c(latex_author, "    ", latex_institution) %>%
  clipr::write_clip()

# Create a list of authors that have not filled the institution field on the Google sheet

authors %>%
  drop_na(email) %>%
  filter(institution == "unidentified") %>%
  pull(email) %>%
  paste0(., collapse = ";") %>%
  clipr::write_clip()

authors %>%
  filter(str_detect(institution, "Asian")) %>%
  pull(email) %>%
  paste0(., collapse = ";") %>%
  clipr::write_clip()
