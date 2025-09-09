#' Plot the Risk Curve of Expected Infections
#'
#' @param disease The disease to model. Default is "Measles".
#' @param kindergarten_size The size of the school cohort. Default is 200.
#' @param VE Vaccine effectiveness. If NULL (the default), a sensible default is
#'   used for the chosen disease (e.g., 0.97 for Measles). A user-provided
#'   value (0 to 1) will override the default.
#' @param r0_custom The basic reproduction number (R0) for a custom disease.
#'   Required only if `disease` is "Custom".
#' @param save_data_to Optional file path to save the plot's underlying data.
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom scales percent
#' @examples
#' # Plot with default VE for Pertussis (~0.85)
#' plot_risk_curve(disease = "Pertussis")
#' # Plot for a custom disease, requires VE and r0_custom
#' plot_risk_curve(disease = "Custom", VE = 0.85, r0_custom = 12)
plot_risk_curve <- function(disease = "Measles", kindergarten_size = 200, VE = NULL, r0_custom = NULL, save_data_to = NULL) {

  # Get the effective VE and R0 to use for calculations
  effective_ve <- get_ve(disease, VE)
  r0 <- get_r0(disease, r0_custom)

  herd_immunity_threshold <- 1 - (1 / r0)

  curve_df <- tibble::tibble(vc = seq(0.75, 1, by = 0.005)) %>%
    dplyr::mutate(
      Expected_Infections = purrr::map_dbl(.data$vc, ~calc_expected_infections(
        vc = ., disease, kindergarten_size, effective_ve, r0_custom
      ))
    )

  subtitle_text <- if (disease == "Custom") {
    paste("For Custom Disease with R0 =", r0, "and VE =", scales::percent(effective_ve, 1))
  } else {
    paste("For", disease, "with R0 =", r0, "and VE =", scales::percent(effective_ve, 1))
  }

  p <- ggplot(curve_df, aes(x = .data$vc, y = .data$Expected_Infections)) +
    geom_line(aes(color = .data$Expected_Infections), linewidth = 1.2) +
    geom_vline(xintercept = herd_immunity_threshold, linetype = "dashed", color = "red") +
    annotate("text", x = herd_immunity_threshold, y = max(curve_df$Expected_Infections) * 0.6,
             label = paste0(scales::percent(herd_immunity_threshold, 0.1), " Herd Immunity"),
             hjust = 1.05, color = "red", angle = 90) +
    scale_x_continuous(labels = scales::percent, name = "Kindergarten Vaccination Coverage Rate") +
    scale_y_continuous(name = paste("Expected Infections (in a school of", kindergarten_size, ")")) +
    scale_color_gradient(low = "green", high = "red", name = "Risk Level") +
    labs(
      title = "The 'Tipping Point' Effect of Vaccination Coverage",
      subtitle = subtitle_text
    ) +
    theme_minimal(base_size = 14)

  if (!is.null(save_data_to)) {
    save_plot_data(curve_df, save_data_to, "Data for Risk Curve Plot")
  }
  return(p)
}


#' Plot Outbreak Probability vs. Coverage
#'
#' @param disease The disease to model. Default is "Measles".
#' @param VE Vaccine effectiveness. If NULL (the default), a sensible default is
#'   used for the chosen disease (e.g., 0.97 for Measles). A user-provided
#'   value (0 to 1) will override the default.
#' @param r0_custom The basic reproduction number (R0) for a custom disease.
#'   Required only if `disease` is "Custom".
#' @param save_data_to Optional file path to save the plot's data.
#' @return A ggplot object.
#' @export
#' @import ggplot2 tidyr
#' @examples
#' # Plot with default VE for Pertussis (~0.85)
#' plot_outbreak_prob(disease = "Pertussis")
#' # Override default VE for Pertussis
#' plot_outbreak_prob(disease = "Pertussis", VE = 0.91)
plot_outbreak_prob <- function(disease = "Measles", VE = NULL, r0_custom = NULL, save_data_to = NULL) {

  effective_ve <- get_ve(disease, VE)

  prob_curve <- tibble::tibble(vc = seq(0.75, 0.99, by = 0.005)) %>%
    dplyr::mutate(
      Re = calc_re(.data$vc, disease, effective_ve, r0_custom),
      `Prob. >=1 Secondary Case` = prob_sec_case(.data$Re),
      `Prob. of Major Outbreak` = prob_major_outbreak(.data$Re)
    )

  plot_df <- prob_curve %>%
    tidyr::pivot_longer(cols = starts_with("Prob."), names_to = "Probability_Type", values_to = "Probability")

  p <- ggplot(plot_df, aes(x = .data$vc, y = .data$Probability, color = .data$Probability_Type)) +
    geom_line(linewidth = 1.2) +
    scale_x_continuous(labels = scales::percent, name = "Kindergarten Vaccination Coverage Rate") +
    scale_y_continuous(labels = scales::percent, name = "Probability") +
    labs(
      title = "Outbreak Probability vs. Vaccination Coverage",
      subtitle = paste0("Model for ", disease, " with VE = ", scales::percent(effective_ve, 1)),
      color = "Probability Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

  if (!is.null(save_data_to)) {
    save_plot_data(prob_curve, save_data_to, "Data for Outbreak Probability Plot")
  }
  return(p)
}


#' Plot Historical Vaccination Coverage Rate
#'
#' @param county_name A character vector of county names to plot.
#' @param save_data_to Optional file path to save the plot's data.
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom dplyr filter
#' @examples
#' plot_coverage_history(county_name = c("Florida", "Miami-Dade", "Liberty"))
plot_coverage_history <- function(county_name, save_data_to = NULL) {
  # Explicitly reference the data from the vaxineR package
  trend_data <- vaxineR::florida_vaccine_coverage %>%
    dplyr::filter(.data$County %in% county_name)

  if (nrow(trend_data) == 0) {
    stop("None of the provided county names were found in the data.")
  }

  p <- ggplot(trend_data, aes(x = .data$Year, y = .data$Coverage_Rate, color = .data$County, group = .data$County)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.2) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
    annotate("text", x = min(trend_data$Year), y = 0.95,
             label = "95% Target Threshold", vjust = -0.5, hjust = 0, color = "red") +
    scale_y_continuous(labels = scales::percent, name = "Vaccination Coverage Rate") +
    scale_x_continuous(breaks = seq(min(trend_data$Year), max(trend_data$Year), by = 1)) +
    labs(
      title = "Historical Vaccination Coverage Trend",
      color = "County/Region"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (!is.null(save_data_to)) {
    save_plot_data(trend_data, save_data_to, "Data for Historical Trend Plot")
  }
  return(p)
}
