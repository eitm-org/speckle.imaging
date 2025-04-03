#' Title plot_raw_bp
#'
#' @param raw_var variable name to plot (should be raw, unnormalized)
#' @param for_plot dataframe named "for_plot" of speckle imaging output cleaned in data-clean_by-nucleus.R
#'
#' @returns
#' @export raw_bp ggplot object boxplot of raw signal by condition
#'
#' @examples
plot_raw_bp <- function(raw_var, for_plot) {
  #filter input dataframe for only the columns you need for this plot
  filt <- for_plot[c(raw_var,
                     "nice_compound2",
                     "control",
                     "exp_date",
                     "treatment_duration")]
  #rename the raw_var column so you can refer to it in your ggplot
  names(filt) <- c("col1",
                   "nice_compound2",
                   "control",
                   "exp_date",
                   "treatment_duration")
  #format raw_var
  pretty_var <- str_replace_all(str_to_title(str_replace_all(
    str_replace_all(raw_var, "_", " "), "nuc", "Per Nucleus"
  )), "Per Nucleuslei", "Nuclei")
  raw_bp <- ggplot(data = filt, aes(x = nice_compound2, y = col1, color = control)) +
    facet_wrap( ~ exp_date + treatment_duration) +
    geom_boxplot() +
    geom_quasirandom(size = 3, alpha = .6) +
    theme_bw() +
    scale_color_manual(values = wes_palette("GrandBudapest2", length(unique(filt$control)), type = "discrete")) +
    ylab(pretty_var) +
    xlab("Condition") +
    ggtitle("Raw Output Values") +
    labs(subtitle = paste(pretty_var, "by Condition")) +
    scale_y_continuous(trans = 'log2') +
    theme(text = element_text(family = "serif"))
  print(raw_bp)
  cat("\n")
  return(raw_bp)
}


#this is different than the original plot_raw_bp
#because 1003 does that thing where they want to compare 3 sets of controls
#' Title plot_raw_bp_1003
#'
#' @param raw_var variable name to plot (should be raw, unnormalized)
#' @param for_plot dataframe named "for_plot" of speckle imaging output cleaned in data-clean_by-nucleus.R
#'
#' @returns
#' @export raw_bp ggplot boxplot of raw signal by condition
#'
#' @examples
#' from plots_by-well.Rmd:
#' bps <- lapply(var_list, plot_raw_bp, for_plot)
#' bps <- lapply(var_list, plot_raw_bp_1003, for_plot)
plot_raw_bp_1003 <- function(raw_var, for_plot) {
  #filter input dataframe for only the columns you need for this plot
  filt <- for_plot[c(
    raw_var,
    "nice_compound2",
    "control",
    "exp_date",
    "reagent_dose_ul",
    "treatment_duration"
  )]
  #rename the raw_var column so you can refer to it in your ggplot
  names(filt) <- c(
    "col1",
    "nice_compound2",
    "control",
    "exp_date",
    "reagent_dose_ul",
    "treatment_duration"
  )
  #format raw_var
  pretty_var <- str_replace_all(str_to_title(str_replace_all(
    str_replace_all(raw_var, "_", " "), "nuc", "Per Nucleus"
  )), "Per Nucleuslei", "Nuclei")
  cat("\n")
  cat(paste("##", pretty_var))
  cat("\n")
  raw_bp <- ggplot(data = filt, aes(x = control, y = col1)) +
    facet_wrap( ~ exp_date + treatment_duration) +
    geom_boxplot() +
    geom_quasirandom(size = 3, alpha = .6, aes(color = as.factor(reagent_dose_ul))) +
    theme_bw() +
    scale_color_nord("afternoon_prarie") +
    ylab(pretty_var) +
    xlab("Condition") +
    ggtitle("Raw Output Values") +
    labs(subtitle = paste(pretty_var, "by Condition"),
         color = "Reagent Dose (ul)") +
    scale_y_continuous(trans = 'log2') +
    theme(text = element_text(family = "serif"))
  print(raw_bp)
  cat("\n")
  cat("\\pagebreak")
  cat("\n")
  return(raw_bp)
}

#i realized that the plot_raw and plot_norm functions are very similar...... welp.
#input:
#
#output:
#' Title plot_norm_bp
#'
#' @param norm_var variable name to plot (should be normalized)
#' @param for_plot dataframe named "for_plot" of speckle imaging output cleaned in data-clean_by-nucleus.R
#'
#' @returns raw_bp ggplot boxplot of raw signal by condition
#' @export
#'
#' @examples
plot_norm_bp <- function(norm_var, for_plot) {
  #filter input dataframe for only the columns you need for this plot
  filt <- for_plot[c(norm_var, "nice_compound2", "control", "exp_date", "r1881")]
  #rename the norm_var column so you can refer to it in your ggplot
  names(filt) <- c("col1", "nice_compound2", "control", "exp_date", "r1881")
  #format norm_var
  pretty_var <- str_to_title(str_replace_all(norm_var, "_", " "))
  raw_bp <- ggplot(data = filt, aes(x = nice_compound2, y = col1, color = control)) +
    facet_wrap( ~ exp_date) +
    geom_boxplot() +
    geom_quasirandom(aes(shape = r1881), size = 2, alpha = .5) +
    theme_bw() +
    scale_color_manual(values = wes_palette("GrandBudapest1", length(unique(filt$control)), type = "discrete")) +
    ylab(pretty_var) +
    xlab("Condition") +
    ggtitle("Normalized Output Values") +
    labs(subtitle = paste(pretty_var, "by Condition")) +
    theme(text = element_text(family = "serif"))
  print(raw_bp)
  cat("\n")
  return(raw_bp)
}

