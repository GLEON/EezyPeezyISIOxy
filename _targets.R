# Load packages required to define the pipeline:
library(targets)
library(tibble)
library(here)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c(
    "tibble", 
    "ncdf4", 
    "readr", 
    "dplyr", 
    "rLakeAnalyzer", 
    "pracma", 
    "LakeMetabolizer", 
    "deSolve",
    "tidyr",
    "lubridate",
    "ggplot2",
    "stringr"
  ), # packages that your targets need to run
  format = "rds" # default storage format
)

# tar_make_clustermq() configuration:
options(clustermq.scheduler = "multicore")

source("R/thermal_script.R")
source("R/thermocline_helper.R")
source("R/oxygen_helper.R")

# target list:
lake_folder <- "ObsDOTest"

lakes <- tibble(
 lake_id = list.files(here(lake_folder), full.names = F)[1:2] 
)

numit <- 5

targets <- tar_map(
  values = lakes,
  tar_target(thermal, thermal_info(here("ObsDOTest", lake_id))),
  tar_target(oxygen, run_oxygen_model(thermal, method = 'rk4', trophy = 'oligo',
                                      iterations = numit)),
  tar_target(plot_oxygen, create_plot(oxygen, lake_id, "ObsDOTest"), format = "file"),
  tar_target(plot_thermal, create_plots_thermal(thermal, lake_id, "ObsDOTest"), format = "file")
  # tar_target(
  #   thermal_to_model,
  #   thermal %>% 
  #     filter(!is.na(stratified)) %>% 
  #     filter(duration > 2) %>% 
  #     group_by(strat_id) %>% 
  #     tar_group(), 
  #   iteration = "group"
  # ),
  # tar_target(oxygen, consume_oxygen(
  #     thermal_to_model, 
  #     method = "rk4", 
  #     trophy = "oligo", 
  #     iterations = 5
  #   ), 
  #   pattern = head(thermal_to_model))
)

list(targets)