rm(list = ls())

# There are no measurements for transect 300.
# Hi Philippe, I did many profiles during that transects, but unfortunately I
# made a mistake with the logger, so nothing was recorded. It sucked. So what
# you have is all the data that there is. Jens

ac9 <- vroom::vroom("data/clean/ac9.csv")

# %>%
#   filter(transect %in% c(600, 300)) %>%
#   filter(station != 345)

ac9 %>%
  distinct(station)

