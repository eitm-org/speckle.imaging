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
  bind_rows() %>%
  filter(!(exp_date == "2024-10-03" & column == "7")) %>%
  #first case when is for 1003
  mutate(reagent_dose_ul = case_when(exp_date == "2024-10-03" & grepl("1.5RNA", compound) ~ "1.5",
                                     #second part is for the first two experiments
                                       TRUE ~ str_extract(compound, "(?<=_)([:digit:]|.)+(?=uLRNA)")),
         r1881 = case_when(grepl("^R1881", compound) ~ "+R1881",
                           TRUE ~ "-R1881"),
         sirna_dose_ul = str_extract(compound, "(?<=RNA_)([:digit:]|.)+(?=uLsiRNA)"),
         gfp_sirna_dose = case_when(exp_date == "2024-10-03" & grepl("0.5 GFP siRNA", compound) ~ "0.5",
                                    TRUE ~ str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=GFP[:blank:]siRNA)")),
         scramble_sirna_dose = str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=Scramble[:blank:]siRNA)"),
         ar_sirna_dose = str_extract(compound, "(?<=RNA[:blank:]\\+[:blank:])([:digit:]|.)+(?=AR[:blank:]siRNA)"))
#all columns except bounding_box and compound should be numeric
dataAR[!(names(dataAR) %in% c("bounding_box", "compound", "r1881", "filepath", "filename", "exp_date"))] <- sapply(dataAR[!(names(dataAR) %in% c("bounding_box", "compound", "r1881", "filepath", "filename", "exp_date"))], as.numeric)

dataAR <- dataAR %>%
  #set sirna_dose_ul = 0 when it is NA (cuz that means there's no siRNA in that compound)
  mutate(sirna_dose_ul = case_when(is.na(sirna_dose_ul) ~ 0,
                              TRUE ~ sirna_dose_ul),
         reagent_dose_ul = case_when(is.na(reagent_dose_ul) ~ 0,
                                   TRUE ~ reagent_dose_ul),
         gfp_sirna_dose = case_when(is.na(gfp_sirna_dose) ~ 0,
                                   TRUE ~ gfp_sirna_dose),
         scramble_sirna_dose = case_when(is.na(scramble_sirna_dose) ~ 0,
                                         TRUE ~ scramble_sirna_dose),
         ar_sirna_dose = case_when(is.na(ar_sirna_dose) ~ 0,
                                   TRUE ~ ar_sirna_dose),
         #designate the conditions for high and low controls
         #1003 had 3 different control experiments they wanted to try
         control = case_when(exp_date == "2024-10-03" & r1881 == "+R1881" & reagent_dose_ul == 1.5 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0 & 
                               ar_sirna_dose == 0 ~ "high\ncontrol\nreagent",
                             exp_date == "2024-10-03" & r1881 == "-R1881" & reagent_dose_ul == 1.5 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0 & 
                               ar_sirna_dose == 0 ~ "low\ncontrol\nreagent",
                             exp_date == "2024-10-03" & r1881 == "+R1881" & reagent_dose_ul == 0 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0 & 
                               ar_sirna_dose == 0 ~ "high\ncontrol\nno treatment",
                             exp_date == "2024-10-03" & r1881 == "-R1881" & reagent_dose_ul == 0 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0 & 
                               ar_sirna_dose == 0 ~ "low\ncontrol\nno treatment",
                             exp_date == "2024-10-03" & r1881 == "+R1881" & reagent_dose_ul == 1.5 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0.5 & 
                               ar_sirna_dose == 0 ~ "high\ncontrol\nscramble",
                             exp_date == "2024-10-03" & r1881 == "-R1881" & reagent_dose_ul == 1.5 & 
                               gfp_sirna_dose == 0 & scramble_sirna_dose == 0.5 & 
                               ar_sirna_dose == 0 ~ "low\ncontrol\nscramble",
                             r1881 == "+R1881" & reagent_dose_ul == 1.5 & sirna_dose_ul == 0 & exp_date != "2024-10-03" ~ "high control",
                             r1881 == "-R1881" & reagent_dose_ul == 1.5 & sirna_dose_ul == 0 & exp_date != "2024-10-03" ~ "low control",
                             TRUE ~ "sample"),
         avg_gfp_nuc = nuclei_intensity_nucleus_alexa_488_mean_mean_per_well/nuclei_number_of_objects) %>%
  #group_by_condition to get percent cv
  group_by(filename, compound, treatment_duration) %>%
  mutate(cv_puncta = sd(average_puncta_nuc)/mean(average_puncta_nuc)*100,
         cv_gfp = sd(avg_gfp_nuc)/mean(avg_gfp_nuc)*100,
         cv_nuc = sd(nuclei_number_of_objects)/mean(nuclei_number_of_objects)*100) %>%
  ungroup()

var_list <- c("average_puncta_nuc", "avg_gfp_nuc", "nuclei_number_of_objects")
#get plate averages for all types of controls
dataARs_ctrl <- c()
for (i in unique(dataAR$control)) {
  if (i == "sample") {
    next
  }
  ctrl <- case_when(grepl("high", i) ~ "hctrl",
                    grepl("low", i) ~ "lctrl",
                    TRUE ~ "")
  xtra <- case_when(grepl("reagent", i) ~ "rgnt",
                    grepl("scramble", i) ~ "scrmbl",
                    grepl("no treatment", i) ~ "ntrt",
                    TRUE ~ "")
  dataARs_var <- c()
  for (j in var_list) {
    var <- case_when(grepl("puncta", j) ~ "avg_puncta",
                     grepl("gfp", j) ~ "gfp",
                     grepl("nuclei", j) ~ "nuc_num")
    new_col_name <- paste("plate", var, ctrl, xtra, sep = "_")
    #if the last character is a _, remove it 
    new_col_name <- str_remove(new_col_name, "_$")
    sd_col <- str_remove(paste("sd", var, ctrl, xtra, sep = "_"), "_$")
    dataARs_file <- c()
    plate_list <- unique(dataAR[,c("filename", "treatment_duration")])
    for (k in 1:nrow(plate_list)) {
      dataAR2 <- dataAR %>% 
        filter(filename == unlist(plate_list[k, "filename"])[[1]] & treatment_duration == unlist(plate_list[k, "treatment_duration"])[[1]])
      dataAR2[new_col_name] = mean(unlist(dataAR2[dataAR2$control == i, j]), na.rm = TRUE)
      dataAR2[sd_col] = sd(unlist(dataAR2[dataAR2$control == i, j]), na.rm = TRUE)
      dataARs_file[[k]] <- dataAR2
    }
    dataARs_var[[j]] <- bind_rows(dataARs_file)
  }
  dataARs_ctrl[[i]] <- reduce(dataARs_var, left_join)
}
dataAR <- reduce(dataARs_ctrl, left_join) 
dataAR <- dataAR %>%
  ungroup() %>%
  mutate(puncta_percent_control = (average_puncta_nuc - plate_avg_puncta_lctrl)/(plate_avg_puncta_hctrl - plate_avg_puncta_lctrl)*100,
         gfp_percent_control = (avg_gfp_nuc - plate_gfp_lctrl)/(plate_gfp_hctrl - plate_gfp_lctrl)*100,
         nice_compound = paste0(sirna_dose_ul, "uL\nsiRNA,\n", reagent_dose_ul, "uL\nreagent,\n", r1881),
         nice_compound_nobreak = paste0(sirna_dose_ul, "uL siRNA, ", reagent_dose_ul, "uL reagent")) %>%
  group_by(filename, treatment_duration) %>%
  mutate(plate_cv_puncta = mean(cv_puncta, na.rm = TRUE),
         plate_cv_gfp = mean(cv_gfp, na.rm = TRUE),
         plate_cv_nuc = mean(cv_nuc, na.rm = TRUE),
         z_prime_puncta = 1 - 3*(sd_avg_puncta_hctrl + sd_avg_puncta_lctrl)/abs(plate_avg_puncta_hctrl - plate_avg_puncta_lctrl),
         z_prime_puncta_rgnt = 1 - 3*(sd_avg_puncta_hctrl_rgnt + sd_avg_puncta_lctrl_rgnt)/abs(plate_avg_puncta_hctrl_rgnt - plate_avg_puncta_lctrl_rgnt),
         z_prime_puncta_scrmbl = 1 - 3*(sd_avg_puncta_hctrl_scrmbl + sd_avg_puncta_lctrl_scrmbl)/abs(plate_avg_puncta_hctrl_scrmbl - plate_avg_puncta_lctrl_scrmbl),
         z_prime_puncta_ntrt = 1 - 3*(sd_avg_puncta_hctrl_ntrt + sd_avg_puncta_lctrl_ntrt)/abs(plate_avg_puncta_hctrl_ntrt - plate_avg_puncta_lctrl_ntrt),
         z_prime_gfp = 1 - 3*(sd_gfp_hctrl + sd_gfp_lctrl)/abs(plate_gfp_hctrl - plate_gfp_lctrl),
         z_prime_gfp_rgnt = 1 - 3*(sd_gfp_hctrl_rgnt + sd_gfp_lctrl_rgnt)/abs(plate_gfp_hctrl_rgnt - plate_gfp_lctrl_rgnt),
         z_prime_gfp_scrmbl = 1 - 3*(sd_gfp_hctrl_scrmbl + sd_gfp_lctrl_scrmbl)/abs(plate_gfp_hctrl_scrmbl - plate_gfp_lctrl_scrmbl),
         z_prime_gfp_ntrt = 1 - 3*(sd_gfp_hctrl_ntrt + sd_gfp_lctrl_ntrt)/abs(plate_gfp_hctrl_ntrt - plate_gfp_lctrl_ntrt),
         z_prime_nuc = 1 - 3*(sd_nuc_num_hctrl + sd_nuc_num_lctrl)/abs(plate_nuc_num_hctrl - plate_nuc_num_lctrl),
         z_prime_nuc_rgnt = 1 - 3*(sd_nuc_num_hctrl_rgnt + sd_nuc_num_lctrl_rgnt)/abs(plate_nuc_num_hctrl_rgnt - plate_nuc_num_lctrl_rgnt),
         z_prime_nuc_scrmbl = 1 - 3*(sd_nuc_num_hctrl_scrmbl + sd_nuc_num_lctrl_scrmbl)/abs(plate_nuc_num_hctrl_scrmbl - plate_nuc_num_lctrl_scrmbl),
         z_prime_nuc_ntrt = 1 - 3*(sd_nuc_num_hctrl_ntrt + sd_nuc_num_lctrl_ntrt)/abs(plate_nuc_num_hctrl_ntrt - plate_nuc_num_lctrl_ntrt))
#make nice_compound_nobreak a factor
dataAR$nice_compound_nobreak <- factor(dataAR$nice_compound_nobreak, levels = unique(arrange(dataAR, sirna_dose_ul, reagent_dose_ul)$nice_compound_nobreak))

checker_df1003 <- unique(dataAR[dataAR$exp_date == "2024-10-03", c("r1881", "reagent_dose_ul", "sirna_dose_ul", "gfp_sirna_dose", "scramble_sirna_dose", "ar_sirna_dose", "compound")])

saveRDS(dataAR, file = here("data", "cleaned", "by_well.RDS"))
