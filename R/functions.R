library(here)
library(tidyverse)
library(readxl)
library(drc)

#' Title read_speckle_xls
#'
#' @param filepath filepath to AR siRNA speckle imaging output excel sheet (copied from dropbox to local drive with dropbox_downloader)
#'
#' @returns dataframe with info from filepath
#' @export
#'
#' @examples
#' dataAR <- data_files %>% map(read_speckle_xls)
read_speckle_xls <- function(filepath) {

  df <- read_excel(filepath,
                   sheet = 1,
                   skip = 10,
                   col_names = TRUE)
  df <- df %>%
    clean_names(replace=janitor:::mu_to_u) %>%
    mutate(
      filepath = filepath,
      filename = str_remove(basename(filepath), ".xlsx"),
      #get 8 numbers sandwiched in between two unfdescores
      exp_date = as.Date(
        str_extract(filepath, "(?<=_)([:digit:]{8})(?=_)"),
        format = "%Y%m%d"
      ),
      #1003 has different timepoints, coded weird
      treatment_duration = case_when(
        exp_date == "2024-10-03" & timepoint == 0 ~ "2",
        exp_date == "2024-10-03" &
          timepoint == 1 ~ "3",
        exp_date == "2024-10-03" &
          timepoint == 2 ~ "6",
        exp_date == "2024-10-03" &
          timepoint == 3 ~ "7",
        TRUE ~ str_extract(filepath, "(?<=_day)([:digit:]+)")
      )
    )
  return(df)
}


#' Title get_yvar_dfs
#'
#' @param yvar the yvariable you want to be the focus of this dataframe (must be the name of a variable in the df dataframe, which is defined outside of this function)
#' @param df dataframe produced by read_speckle_xls() and data cleaning in data-clean_by_nucleus.R
#'
#' @returns gives you a dataframe to use for each measure ken wants to examine as a potential yvariable for our analysis
#' @export
#'
#' @examples
#' yvar_df <- yvar_list %>% map(get_yvar_dfs, dataAR)
get_yvar_dfs <- function(yvar, df) {
  df <- df %>%
    dplyr::select(
      c("row",
        "column",
        "timepoint",
        "compound",
        "concentration",
        "cell_type",
        "filepath",
        "filename",
        "exp_date",
        "treatment_duration",
        "reagent_dose_ul",
        "r1881",
        "ar_sirna_dose_ul",
        "gfp_sirna_dose",
        "scramble_sirna_dose",
        "control",
        yvar
      )
    )
  df["yvar"] <- df[yvar]
  df["variable"] <- yvar
  return(df)
}

getDRM <- function(.x) {
  tryCatch({
    drm(puncta_normalized ~ concentration,
        data = .x,
        fct = LL.4())
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}
