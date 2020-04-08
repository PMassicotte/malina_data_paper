#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load packages and setup for the project.
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(tidyverse)
library(glue)
library(dbplyr)
library(DBI)
library(RSQLite)
library(ggspatial)
library(assertr)
library(ggpmthemes)
library(data.table)
library(assertr)
library(sf)
library(MBA)
library(ggisoband)
library(patchwork)

## Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_poppins(base_size = 10))

# Scripts -----------------------------------------------------------------

source("R/sqlite_to_csv.R")

source("R/clean_stations.R")

source("R/clean_optic_anap.R")
source("R/clean_optic_aphy.R")
source("R/clean_optic_atot.R")
source("R/clean_optic_bb_bbp.R")
source("R/clean_optic_eu.R")

source("R/fig01.R")
