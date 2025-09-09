#' Summarize Vaccination Coverage for a Given Year
#'
#' Retrieves and sorts the vaccination coverage rates for all Florida counties
#' for a specified year.
#'
#' @param yr The year to summarize (e.g., 2024).
#' @return A tibble with columns `County` and `Coverage_Rate` (as a numeric
#'   proportion), sorted in descending order of the coverage rate.
#' @export
#' @importFrom dplyr filter select arrange desc .data
#' @examples
#' # Get a summary of vaccine coverage for 2024
#' summary_coverage(yr = 2024)
summary_coverage <- function(yr) {
  vaxineR::florida_vaccine_coverage %>%
    dplyr::filter(.data$Year == yr) %>%
    dplyr::select(.data$County, .data$Coverage_Rate) %>%
    dplyr::arrange(dplyr::desc(.data$Coverage_Rate))
}


#' Generate an Outbreak Risk Simulation Table
#'
#' Creates a summary table of outbreak risk for a given year. The analysis
#' includes scenarios for the statewide average coverage rate as well as for
#' counties at the minimum, maximum, and quartile coverage rates. This function
#' can model pre-set diseases or a custom disease with a user-specified R0.
#'
#' @param yr The year for the analysis (e.g., 2024).
#' @param disease The disease to model. Choose from "Measles", "Pertussis",
#'   "Chickenpox", or "Custom". Default is "Measles".
#' @param kindergarten_size The size of the school cohort. Default is 200.
#' @param VE Vaccine effectiveness. If NULL (the default), a sensible default is
#'   used for the chosen disease (e.g., 0.97 for Measles). A user-provided
#'   value (0 to 1) will override the default.
#' @param r0_custom The basic reproduction number (R0) for a custom disease.
#'   This parameter is required and must be a positive number if `disease` is set to "Custom".
#' @return A data frame containing the formatted risk analysis table.
#' @export
#' @import dplyr
#' @importFrom stats quantile
#' @importFrom purrr map_dbl
#' @importFrom scales percent
#' @importFrom tibble tibble
#' @examples
#' # Example 1: Standard analysis for Measles in 2024 (uses default VE of 0.97)
#' summary_infection_risk(yr = 2024, disease = "Measles")
#'
#' # Example 2: Analysis for Pertussis, overriding the default VE
#' summary_infection_risk(yr = 2024, disease = "Pertussis", VE = 0.91)
#'
#' # Example 3: Analysis for a custom disease (e.g., Mumps-like)
#' summary_infection_risk(yr = 2024, disease = "Custom", VE = 0.88, r0_custom = 11)
summary_infection_risk <- function(yr, disease = "Measles", kindergarten_size = 200, VE = NULL, r0_custom = NULL) {

  effective_ve <- get_ve(disease, VE)

  data_yr <- vaxineR::florida_vaccine_coverage %>%
    dplyr::filter(.data$Year == yr, .data$County != "Florida")

  state_avg <- vaxineR::florida_vaccine_coverage %>%
    dplyr::filter(.data$Year == yr, .data$County == "Florida")

  quantiles <- stats::quantile(data_yr$Coverage_Rate, probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)

  scenarios_df <- tibble::tibble(
    label = c("Statewide Average", "Minimum", "25th Percentile", "Median", "75th Percentile", "Maximum"),
    vc = c(state_avg$Coverage_Rate[1], quantiles)
  )

  # Removed the intermediate 's' variable to resolve the NOTE.
  results <- scenarios_df %>%
    dplyr::mutate(
      Re = calc_re(.data$vc, disease = disease, VE = effective_ve, r0_custom = r0_custom),
      Susceptible_in_School = ceiling(kindergarten_size * (1 - (.data$vc * effective_ve))),
      Expected_Infections = purrr::map_dbl(.data$vc, ~calc_expected_infections(
        vc = .,
        disease = disease,
        kindergarten_size = kindergarten_size,
        VE = effective_ve,
        r0_custom = r0_custom
      )),
      Prob_Secondary = prob_sec_case(.data$Re),
      Prob_Major = prob_major_outbreak(.data$Re)
    )

  results %>%
    dplyr::select(
      Scenario = .data$label,
      `Vaccination Coverage` = .data$vc,
      `Effective R (Re)` = .data$Re,
      `Susceptible (N = 200)` = .data$Susceptible_in_School,
      `Expected Infections` = .data$Expected_Infections,
      `Prob >=1 Secondary Case` = .data$Prob_Secondary,
      `Prob Major Outbreak` = .data$Prob_Major
    ) %>%
    dplyr::mutate(
      `Vaccination Coverage` = scales::percent(.data$`Vaccination Coverage`, accuracy = 0.1),
      `Prob >=1 Secondary Case` = scales::percent(.data$`Prob >=1 Secondary Case`, accuracy = 0.1),
      `Prob Major Outbreak` = scales::percent(.data$`Prob Major Outbreak`, accuracy = 0.1),
      `Effective R (Re)` = round(.data$`Effective R (Re)`, 2)
    )
}
