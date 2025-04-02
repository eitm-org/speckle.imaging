library(readxl)
library(tidyverse)
library(janitor)
#^this package has a function called clean_names which I love
library(stringr)
library(here)

source(here("functions.R"))

data_path <- here("data", "unzipped")
#get list of files, but not directories
data_files <- setdiff(list.files(data_path, full.names = TRUE, recursive = FALSE), list.dirs(data_path, full.names = TRUE, recursive = FALSE))
#remove the 2x mutant file
data_files <- data_files[!grepl("mutant", data_files) & 
                           #only read in excel files
                           grepl(".xls", data_files) & 
                           #don't try to read hidden files
                           !grepl("~", data_files)]
#only use files with "newformula" in the titles for older files
#TODO: put older files in "archive" folder in DROPBOX so you don't have to do this
data_files <- data_files[grepl("newformula", data_files) | grepl("2024102", data_files)]
#read in files, add columns
dataAR <- data_files %>%
  map(read_speckle_xls) %>%
  bind_rows() 
#separated this because read_speckle_xls takes a long time to run
dataAR <- dataAR %>%
  filter(!(exp_date == "2024-10-03" & column == "7")) %>%
  #first case when is for 1003
  mutate(reagent_dose_ul = case_when(grepl("1.5RNA|1.5uLRNA", compound) ~ "1.5",
                                     #second part is for the first two experiments
                                       TRUE ~ str_extract(compound, "(?<=_)([:digit:]|.)+(?=uLRNA)")),
         r1881 = case_when(grepl("^R1881| R1881", compound) ~ "+R1881",
                           TRUE ~ "-R1881"),
         ar_sirna_dose_ul = str_extract(compound, "(?<=RNA_)([:digit:]|.)+(?=uLsiRNA)"),
         gfp_sirna_dose = case_when(grepl("0.5uLGFPsiRNA", compound) ~ "0.5",
                                    TRUE ~ str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=uLGFPsiRNA)")),
         scramble_sirna_dose = str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=uLscramblesiRNA)")) %>%
  mutate(ar_sirna_dose_ul = case_when(is.na(ar_sirna_dose_ul) & exp_date == "2024-10-03" ~ str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=AR[:blank:]siRNA)"),
                                      is.na(ar_sirna_dose_ul) ~ str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=uLARsiRNA)"),
                                      TRUE ~ ar_sirna_dose_ul),
         scramble_sirna_dose = case_when(is.na(scramble_sirna_dose) & exp_date == "2024-10-03" ~ str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=Scramble[:blank:]siRNA)"),
                                         TRUE ~ scramble_sirna_dose))
         # ar_sirna_dose_ul = str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=AR[:blank:]siRNA)")) %>%
#all columns except bounding_box and compound should be numeric
dataAR[!(names(dataAR) %in% c("bounding_box", "compound", "r1881", "filepath", "filename", "exp_date"))] <- sapply(dataAR[!(names(dataAR) %in% c("bounding_box", "compound", "r1881", "filepath", "filename", "exp_date"))], as.numeric)

dataAR <- dataAR %>%
  #set sirna_dose_ul = 0 when it is NA (cuz that means there's no siRNA in that compound)
  mutate(ar_sirna_dose_ul = case_when(is.na(ar_sirna_dose_ul) ~ 0,
                              TRUE ~ ar_sirna_dose_ul),
         reagent_dose_ul = case_when(is.na(reagent_dose_ul) ~ 0,
                                   TRUE ~ reagent_dose_ul),
         gfp_sirna_dose = case_when(is.na(gfp_sirna_dose) ~ 0,
                                   TRUE ~ gfp_sirna_dose),
         scramble_sirna_dose = case_when(is.na(scramble_sirna_dose) ~ 0,
                                         TRUE ~ scramble_sirna_dose),
         #designate the conditions for high and low controls
         #1003 had 3 different control experiments they wanted to try
         control = case_when(grepl("GFP", compound) ~ "sample",
                             r1881 == "+R1881" & reagent_dose_ul == 1.5 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0 & 
                               ar_sirna_dose_ul == 0 ~ "high\ncontrol\nreagent",
                             r1881 == "-R1881" & reagent_dose_ul == 1.5 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0 & 
                               ar_sirna_dose_ul == 0 ~ "low\ncontrol\nreagent",
                             r1881 == "+R1881" & reagent_dose_ul == 0 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0 & 
                               ar_sirna_dose_ul == 0 ~ "high\ncontrol\nno treatment",
                             r1881 == "-R1881" & reagent_dose_ul == 0 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0 & 
                               ar_sirna_dose_ul == 0 ~ "low\ncontrol\nno treatment",
                             r1881 == "+R1881" & reagent_dose_ul == 1.5 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0.5 & 
                               ar_sirna_dose_ul == 0 ~ "high\ncontrol\nscramble",
                             r1881 == "-R1881" & reagent_dose_ul == 1.5 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0.5 & 
                               ar_sirna_dose_ul == 0 ~ "low\ncontrol\nscramble",
                             # r1881 == "+R1881" & reagent_dose_ul == 1.5 & ar_sirna_dose_ul == 0 & exp_date != "2024-10-03" ~ "high control",
                             # r1881 == "-R1881" & reagent_dose_ul == 1.5 & ar_sirna_dose_ul == 0 & exp_date != "2024-10-03" ~ "low control",
                             TRUE ~ "sample")) %>%
  ungroup()
#group by well to subsample and get by-well stats
cell_counts <- dataAR %>%
  group_by(filename, row, column, exp_date, treatment_duration) %>%
  summarize(n_cells = n(), .groups = "keep")

#separate measurements into 3 different dataframes
#because making all these into columns in one dataframe is too much!
#i'm calling them variables or VAR because that's short
yvar_list <- c("nuclei_number_of_spots", "nuclei_ratio_hyperspeckling", "nuclei_ratio_nucleus_cell")
#the get_yvar_dfs function takes dataAR and adds a new column called "yvar" 
  #(and removes columns we don't need to save space)
  #yvar is set to whatever variable this dataframe is named after
  #so now we can loop through this list and normalize and plot each yvar with the same functions!
yvar_df <- yvar_list %>%
  map(get_yvar_dfs) %>%
  bind_rows
#subsample each well
subsample_to <- min(cell_counts$n_cells[cell_counts$n_cells != 1])
subsamp_df <- yvar_df %>%
  group_by(filename, row, column, variable) %>%
  slice_sample(n = subsample_to) %>%
  mutate(ctrl_cond = case_when(control == "sample" ~ control,
                               TRUE ~ str_extract(control, "(?<=\\n)([A-Za-z- ]+$)"))) %>%
  ungroup()
  
bywell_df <- subsamp_df %>%
  #spy = the dye they use to see the stuff
  #if a well has no dye, it has like really low readings
  #and shouldn't be used to calculate qc stats
  filter(grepl("SPY", compound)) %>%
  #group by well
  #i put a lot of columns in this group_by because we want
  #to keep all these around after the summarize statement
  group_by(row, column, timepoint, compound, concentration, 
           filepath, filename, exp_date, treatment_duration, 
           reagent_dose_ul, r1881, ar_sirna_dose_ul, gfp_sirna_dose, 
           scramble_sirna_dose, control, ctrl_cond, variable) %>%
  mutate(sum_yvar = case_when(grepl("ratio", variable) ~ mean(yvar, na.rm = TRUE),
                                 TRUE ~ sum(yvar, na.rm = TRUE)),
            se_yvar = case_when(grepl("ratio", variable) ~ sd(yvar, na.rm = TRUE),
                                TRUE ~ NA)) %>%
  ungroup() %>%
  distinct(row, column, timepoint, compound, concentration, 
           filepath, filename, exp_date, treatment_duration, 
           reagent_dose_ul, r1881, ar_sirna_dose_ul, gfp_sirna_dose, 
           scramble_sirna_dose, control, ctrl_cond, variable, sum_yvar, se_yvar) %>%
  #group by condition to get %cvs
  group_by(timepoint, compound, concentration, 
           filepath, filename, exp_date, treatment_duration,
           variable) %>%
  mutate(cv = sd(sum_yvar)/mean(sum_yvar)*100) %>%
  ungroup() %>%
  #group by plate to get plate %cvs
  group_by(timepoint, filepath, filename, exp_date, treatment_duration, variable) %>%
  mutate(plate_cv = mean(cv, na.rm = TRUE)) %>%
ungroup() %>%
#group_by plate, ctrl cond, variable
group_by(filepath, filename, treatment_duration, exp_date, variable, ctrl_cond) %>%
mutate(avg_hctrl = case_when(control == "sample" ~ NA,
                             TRUE ~ mean(sum_yvar[grepl("high", control)], na.rm = TRUE)),
       avg_lctrl = case_when(control == "sample" ~ NA,
                             TRUE ~ mean(sum_yvar[grepl("low", control)], na.rm = TRUE)),
       sd_hctrl = case_when(control == "sample" ~ NA,
                            TRUE ~ sd(sum_yvar[grepl("high", control)], na.rm = TRUE)),
       sd_lctrl = case_when(control == "sample" ~ NA,
                            TRUE ~ sd(sum_yvar[grepl("low", control)], na.rm = TRUE)),
       zprime = case_when(control == "sample" ~ NA,
                          TRUE ~ 1 - (3*sd_hctrl + 3*sd_lctrl)/abs(avg_hctrl - avg_lctrl))) %>%
  ungroup()

checker_df0814 <- unique(dataAR[dataAR$exp_date == "2024-08-14", 
                                c("r1881", "reagent_dose_ul", "ar_sirna_dose_ul", "gfp_sirna_dose", 
                                  "scramble_sirna_dose", "compound", "control")])
checker_df1003 <- bywell_df %>%
  filter(exp_date == "2024-10-03") %>%
  group_by(r1881, reagent_dose_ul, ar_sirna_dose_ul, gfp_sirna_dose, scramble_sirna_dose, compound, control) %>%
  summarize(count = n())
checker_df1021 <- unique(dataAR[dataAR$exp_date == "2024-10-21", 
                                c("r1881", "reagent_dose_ul", "ar_sirna_dose_ul", "gfp_sirna_dose",
                                  "scramble_sirna_dose", "compound", "control")])
checker_df1022 <- unique(dataAR[dataAR$exp_date == "2024-10-22", 
                                c("r1881", "reagent_dose_ul", "ar_sirna_dose_ul", "gfp_sirna_dose", 
                                  "scramble_sirna_dose", "compound", "control")])

saveRDS(cell_counts, file = here("data", "cleaned", "cell_counts.RDS"))
saveRDS(yvar_df, file = here("data", "cleaned", "by_nucleus.RDS"))
saveRDS(subsamp_df, file = here("data", "cleaned", "by_nucleus_subsampled.RDS"))
saveRDS(bywell_df, file = here("data", "cleaned", "by_nucleus_subsampled_summarized.RDS"))

