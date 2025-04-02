
#input: filepath to AR siRNA speckle imaging output excel sheet
#output: dataframe with info from filepath
read_speckle_xls <- function(filepath) {
  # if (grepl("20241003", filepath)) {
  #   df <- read_excel(filepath, sheet = 1, skip = 9, col_names = TRUE) 
  # } else {
  #   df <- read_excel(filepath, sheet = 2, skip = 9, col_names = TRUE) 
  # }
  
  df <- read_excel(filepath, sheet = 1, skip = 10, col_names = TRUE) 
  df <- df %>%
    clean_names() %>%
    mutate(filepath = filepath,
           filename = str_remove(basename(filepath), ".xlsx"),
           #get 8 numbers sandwiched in between two unfdescores
           exp_date = as.Date(str_extract(filepath, "(?<=_)([:digit:]{8})(?=_)"), format = "%Y%m%d"),
           #1003 has different timepoints, coded weird
           treatment_duration = case_when(exp_date == "2024-10-03" & timepoint == 0 ~ "2",
                                          exp_date == "2024-10-03" & timepoint == 1 ~ "3",
                                          exp_date == "2024-10-03" & timepoint == 2 ~ "6",
                                          exp_date == "2024-10-03" & timepoint == 3 ~ "7",
                                          TRUE ~ str_extract(filepath, "(?<=_day)([:digit:]+)")))
  return(df)
}

#input: dataframe named "for_plot" of speckle imaging output cleaned in data-clean_by-nucleus.R
        #variable name to plot (should be raw, unnormalized)
#output: boxplot of raw signal by condition
plot_raw_bp <- function(raw_var, for_plot) {
  #filter input dataframe for only the columns you need for this plot
  filt <- for_plot[c(raw_var, "nice_compound2", "control", "exp_date", "treatment_duration")]
  #rename the raw_var column so you can refer to it in your ggplot
  names(filt) <- c("col1", "nice_compound2", "control", "exp_date", "treatment_duration")
  #format raw_var
  pretty_var <- str_replace_all(str_to_title(str_replace_all(str_replace_all(raw_var, "_", " "), "nuc", "Per Nucleus")), "Per Nucleuslei", "Nuclei")
  raw_bp <- ggplot(data = filt, aes(x = nice_compound2, y = col1, color = control)) +
    facet_wrap(~exp_date + treatment_duration) +
    geom_boxplot() +
    geom_quasirandom(size = 3, alpha = .6) +
    theme_bw() +
    scale_color_manual(values = wes_palette("GrandBudapest2", length(unique(filt$control)), type = "discrete")) +
    ylab(pretty_var) +
    xlab("Condition") +
    ggtitle("Raw Output Values") +
    labs(subtitle = paste(pretty_var, "by Condition")) +
    scale_y_continuous(trans='log2') +
    theme(text=element_text(family="serif"))
  print(raw_bp)
  cat("\n")
  return(raw_bp)
}

#input: dataframe named "for_plot" of speckle imaging output cleaned in data-clean_by-nucleus.R
#variable name to plot (should be raw, unnormalized)
#output: boxplot of raw signal by condition

#this is different than the original plot_raw_bp
#because 1003 does that thing where they want to compare 3 sets of controls
plot_raw_bp_1003 <- function(raw_var, for_plot) {
  #filter input dataframe for only the columns you need for this plot
  filt <- for_plot[c(raw_var, "nice_compound2", "control", "exp_date", "reagent_dose_ul", "treatment_duration")]
  #rename the raw_var column so you can refer to it in your ggplot
  names(filt) <- c("col1", "nice_compound2", "control", "exp_date", "reagent_dose_ul", "treatment_duration")
  #format raw_var
  pretty_var <- str_replace_all(str_to_title(str_replace_all(str_replace_all(raw_var, "_", " "), "nuc", "Per Nucleus")), "Per Nucleuslei", "Nuclei")
  cat("\n")
  cat(paste("##", pretty_var))
  cat("\n")
  raw_bp <- ggplot(data = filt, aes(x = control, y = col1)) +
    facet_wrap(~ exp_date + treatment_duration) +
    geom_boxplot() +
    geom_quasirandom(size = 3, alpha = .6, aes(color = as.factor(reagent_dose_ul))) +
    theme_bw() +
    scale_color_nord("afternoon_prarie") +
    ylab(pretty_var) +
    xlab("Condition") +
    ggtitle("Raw Output Values") +
    labs(subtitle = paste(pretty_var, "by Condition"), 
         color = "Reagent Dose (ul)") +
    scale_y_continuous(trans='log2') +
    theme(text=element_text(family="serif"))
  print(raw_bp)
  cat("\n")
  cat("\\pagebreak")
  cat("\n")
  return(raw_bp)
}

#i realized that the plot_raw and plot_norm functions are very similar...... welp.
#input: dataframe named "for_plot" of speckle imaging output cleaned in data-clean_by-nucleus.R
  #variable name to plot (should be normalized)
#output: boxplot of raw signal by condition
plot_norm_bp <- function(norm_var, for_plot) {
  #filter input dataframe for only the columns you need for this plot
  filt <- for_plot[c(norm_var, "nice_compound2", "control", "exp_date", "r1881")]
  #rename the norm_var column so you can refer to it in your ggplot
  names(filt) <- c("col1", "nice_compound2", "control", "exp_date", "r1881")
  #format norm_var
  pretty_var <- str_to_title(str_replace_all(norm_var, "_", " "))
  raw_bp <- ggplot(data = filt, aes(x = nice_compound2, y = col1, color = control)) +
    facet_wrap(~exp_date) +
    geom_boxplot() +
    geom_quasirandom(aes(shape = r1881), size = 2, alpha = .5) +
    theme_bw() +
    scale_color_manual(values = wes_palette("GrandBudapest1", length(unique(filt$control)), type = "discrete")) +
    ylab(pretty_var) +
    xlab("Condition") +
    ggtitle("Normalized Output Values") +
    labs(subtitle = paste(pretty_var, "by Condition")) +
    theme(text=element_text(family="serif"))
  print(raw_bp)
  cat("\n")
  return(raw_bp)
}

#this gives you a dataframe to use for each measure ken wants to examine
  #as a potential yvariable for our analysis
get_yvar_dfs <- function(yvar) {
  df <- dataAR %>%
    dplyr::select(c("row", "column", "timepoint", "compound", "concentration", "cell_type", 
                    "filepath", "filename", "exp_date", "treatment_duration", "reagent_dose_ul", 
                    "r1881", "ar_sirna_dose_ul", "gfp_sirna_dose", "scramble_sirna_dose", "control", yvar))
  df["yvar"] <- df[yvar]
  df["variable"] <- yvar
  return(df)
}

getDRM <- function(.x) {
  
  tryCatch({
    drm(puncta_normalized ~ concentration, data = .x, fct = llogistic())
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}
