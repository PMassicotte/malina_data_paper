# Overview of the psicam absorption data.

read_psicam <- function(file) {

  print(file)

  # file <- "~/Downloads/PSICAMforDataBase/Leg2b/ProcessedData/PSICAM-620.dat"
  dat <- read_lines(file)

  i <- which(grepl('^"wave"', dat)) - 1

  df <-
    read_table2(
      file,
      skip = i,
      guess_max = 1e5
    ) %>%
    janitor::clean_names()


  df <- data.table(df) %>%
    rowid_to_column(var = "id") %>%
    melt(
      id.vars = "id",
      measure.vars = patterns(
        wave = "wave",
        cdom = "cdom",
        total = "total"
      )
    ) %>%
    pivot_longer(cols = c("cdom", "total"), names_to = "type", values_to = "absorption") %>%
    mutate(filename = basename(file))

  return(df)
}

files <-
  fs::dir_ls(
    "~/Downloads/PSICAMforDataBase/Leg2b/ProcessedData/",
    glob = "*.dat",
    recurse = TRUE
  )

df <- map_df(files, read_psicam)

df %>%
  filter(wave <= 600) %>%
  ggplot(aes(x = wave, y = absorption, group = interaction(variable, type), color = type)) +
  geom_line() +
  facet_wrap(~filename, scales = "free")
