df <- vroom::vroom(
  "/home/filoche/Downloads/RES3_IOP_CTD_Barge__2010-12-07/res3_archive_136_1m.txt",
  .name_repair = "minimal"
) %>%
  janitor::clean_names() %>%
  select(1:7, matches("\\d{3}$")) %>%
  pivot_longer(matches("\\d{3}$")) %>%
  separate(name,
    into = c("variable", "wavelength"),
    convert = TRUE
  ) %>%
  pivot_wider(names_from = variable, values_from = value)

df

df %>%
  drop_na(c) %>%
  ggplot(aes(x = c, y = pres_dbar, color = factor(wavelength))) +
  geom_path() +
  scale_y_reverse()
