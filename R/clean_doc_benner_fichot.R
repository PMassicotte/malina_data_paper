# Clean updated DOC data given by Cedric Fichot. This is the same data as in the
# "ctd.csv" file that was originally included in the clean database. This new
# data contains 1 more column.

rm(list = ls())

doc_ctd <- read_excel(
  "data/raw/new_data/MALINA-BennerLab_data_Ship-Based.xlsx",
  skip = 5,
  na = c("", "NaN")
) %>%
  janitor::clean_names(replace = c("º" = "degree", "µ" = "micro"))

doc_ctd

doc_ctd %>%
  distinct(long, lat, .keep_all = TRUE) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = station))

doc_zodiac <- read_excel(
  "data/raw/new_data/MALINA-BennerLab_data_ZodiacTransects.xls",
  skip = 7,
  na = c("", "NaN"),
  col_names = c(
    "date",
    "station",
    "latitude",
    "longitude",
    "station_depth_m",
    "sample_depth_m",
    "temp_degree_c",
    "salinity",
    "doc_micromol_l",
    "doc_sd_micromol_l",
    "tdn_micromol_l",
    "tdn_sd_micromol_l",
    "thaa_nmol_l",
    "tdlp9_nmol_l"
  )
) %>%
  filter(str_detect(station, "^\\d{3}$")) %>%
  type_convert()

doc_zodiac
