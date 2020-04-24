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
library(assertr)
library(ggpmthemes)
library(data.table)
library(assertr)
library(sf)
library(MBA)
library(ggisoband)
library(patchwork)
library(readxl)

## Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_poppins(base_size = 10))

# Data cleaning -----------------------------------------------------------

source("R/sqlite_to_csv.R")

source("R/clean_stations.R")

source("R/clean_optic_anap.R")
source("R/clean_optic_aphy.R")
source("R/clean_optic_atot.R")
source("R/clean_optic_bb_bbp.R")
source("R/clean_optic_eu.R")
source("R/clean_ctd_jens.R")

# Figures -----------------------------------------------------------------

source("R/fig01.R")
source("R/fig02.R")
source("R/fig03.R")
source("R/fig04.R")
source("R/fig05.R")
source("R/fig06.R")
source("R/fig07.R")
source("R/fig08.R")
source("R/fig09.R")
source("R/fig10.R")
