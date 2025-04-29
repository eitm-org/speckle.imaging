library(readxl)
library(tidyverse)
library(janitor)
library(stringr)
library(here)
library(drc)
source(here("R", "functions.R"))

data_path <- here("input_data", "unzipped")
#get list of files, but not directories
data_files <- setdiff(list.files(data_path, full.names = TRUE, recursive = FALSE), list.dirs(data_path, full.names = TRUE, recursive = FALSE))
#only read in the 1725 files
data_files <- data_files[grepl("1725", data_files) &
                           #only read in excel files
                           grepl(".xls", data_files) &
                           #don't try to read hidden files
                           !grepl("~", data_files)]
dataAR <- data_files %>%
  map(read_speckle_xls) %>%
  bind_rows() %>%
  #add experiment IDs
  mutate(exp_id = case_when(grepl("1725M", filename) ~ "1725M",
                            grepl("1725R", filename) ~ "1725R",
                            grepl("1725S", filename) ~ "1725S"))

#all columns except bounding_box and compound should be numeric
dataAR[!(names(dataAR) %in% c("bounding_box", "exp_id", "cell_type", "compound", "r1881", "filepath", "filename", "exp_date"))] <- sapply(dataAR[!(names(dataAR) %in% c("cell_type", "exp_id", "bounding_box", "compound", "r1881", "filepath", "filename", "exp_date"))], as.numeric)

#group by well to subsample and get by-well stats
 cell_counts <- dataAR %>%
  group_by(filename, row, column, exp_date, treatment_duration, exp_id) %>%
  summarize(n_cells = n(), .groups = "keep")

subsample_to <- min(cell_counts$n_cells[cell_counts$n_cells != 1])

subsamp_df <- dataAR %>%
  group_by(filename, row, column, exp_id) %>%
  slice_sample(n = subsample_to)

bywell_df <- subsamp_df %>%
  group_by(row, column, timepoint, compound, concentration,
           filepath, filename, exp_date, treatment_duration, exp_id) %>%
  mutate(subsamp_puncta = sum(nuclei_number_of_spots, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(row, column, timepoint, compound, concentration,
           filepath, filename, exp_date, treatment_duration, subsamp_puncta, exp_id) %>%
  group_by(timepoint, compound, concentration,
           filepath, filename, exp_date, treatment_duration, exp_id) %>%
  mutate(cv = sd(subsamp_puncta)/mean(subsamp_puncta)*100) %>%
  ungroup() %>%
  group_by(filepath, filename, exp_date, treatment_duration, exp_id) %>%
  mutate(plate_cv = mean(cv, na.rm = TRUE)) %>%
  mutate(ctrl = case_when(compound == "DMSO + R881" ~ "low control",
                          compound == "DMSO - R1881" ~ "high control",
                          TRUE ~ "sample")) %>%
  group_by(filepath, filename, treatment_duration, exp_date) %>%
  mutate(avg_hctrl = mean(subsamp_puncta[ctrl == "high control"], na.rm = TRUE),
         avg_lctrl = mean(subsamp_puncta[ctrl == "low control"], na.rm = TRUE),
         sd_hctrl = sd(subsamp_puncta[ctrl == "high control"], na.rm = TRUE),
         sd_lctrl = sd(subsamp_puncta[ctrl == "low control"], na.rm = TRUE),
         zprime = 1 - (3*sd_hctrl + 3*sd_lctrl)/abs(avg_hctrl - avg_lctrl)) %>%
  ungroup() %>%
  mutate(puncta_normalized = (subsamp_puncta - avg_lctrl)/(avg_hctrl - avg_lctrl)*100)

if (!dir.exists(here("input_data", "cleaned"))) {
  if (!dir.exists(here("input_data"))) {
    dir.create(here("input_data"))
  }
  dir.create(here("input_data", "cleaned"))
}
saveRDS(bywell_df, file = here("input_data", "cleaned", "by_nucleus_subsamp_1725.RDS"))
saveRDS(cell_counts, file = here("input_data", "cleaned", "by_nucleus_cell_counts_1725.RDS"))

