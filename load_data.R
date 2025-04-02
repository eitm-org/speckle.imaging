library(readxl)
library(tidyverse)
library(janitor)
library(stringr)
library(here)

data_path <- dropbox_downloader("https://www.dropbox.com/scl/fo/36jnhue6abszkq2jegjfv/AGRQvghYn6TpCx8uY2KCjZ8?rlkey=4l4xa8nofqtjgojf26xg18p76&e=3&st=a56hmhn0&unfurl=slack_gen&dl=0")
#need a new dropbox folder to do this in

# data_path <- here("data", "unzipped")
# #get list of files, but not directories
# data_files <- setdiff(list.files(data_path, full.names = TRUE, recursive = FALSE), list.dirs(data_path, full.names = TRUE, recursive = FALSE))
# #remove the 2x mutant file
# data_files <- data_files[!grepl("mutant", data_files) &
#                            #only read in excel files
#                            grepl(".xls", data_files) &
#                            #don't try to read hidden files
#                            !grepl("~", data_files)]
# #only use files with "newformula" in the titles for older files
# #TODO: put older files in "archive" folder in DROPBOX so you don't have to do this
# data_files <- data_files[grepl("1725", data_files)]
