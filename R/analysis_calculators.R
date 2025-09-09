# Helper function to get R0 from disease name (not exported)
get_r0 <- function(disease, r0_custom = NULL) {
  # Standard disease R0 values
  disease_r0_map <- c("Measles" = 15, "Pertussis" = 14, "Chickenpox" = 10)
  # Handle "Custom" disease
  if (disease == "Custom") {
    if (is.null(r0_custom) || !is.numeric(r0_custom) || r0_custom <= 0) {
      stop("For disease = 'Custom', you must provide a positive numeric value for 'r0_custom'.", call. = FALSE)
    }
    return(r0_custom)
  }
  # Handle standard diseases
  r0 <- disease_r0_map[disease]
  if (is.na(r0)) {
    stop("Disease not recognized. Please choose from 'Measles', 'Pertussis', 'Chickenpox', or 'Custom'.", call. = FALSE)
  }
  return(r0)
}

# NEW: Helper function to get VE from disease name (not exported)
# It uses a sensible default if VE is not provided by the user for a standard disease.
get_ve <- function(disease, ve_user = NULL) {
  # If user provides a VE, validate and use it. This always takes precedence.
  if (!is.null(ve_user)) {
    if (!is.numeric(ve_user) || ve_user < 0 || ve_user > 1) {
      stop("If provided, 'VE' must be a numeric value between 0 and 1.", call. = FALSE)
    }
    return(ve_user)
  }

  # If user does NOT provide a VE, use defaults for standard diseases.
  disease_ve_map <- c("Measles" = 0.97, "Pertussis" = 0.85, "Chickenpox" = 0.85)

  if (disease == "Custom") {
    stop("For disease = 'Custom', you must provide a value for the 'VE' parameter.", call. = FALSE)
  }

  ve_default <- disease_ve_map[disease]

  if (is.na(ve_default)) {
    stop("Disease not recognized. Please choose from 'Measles', 'Pertussis', 'Chickenpox', or 'Custom'.", call. = FALSE)
  }

  return(ve_default)
}


#' Calculate the effective reproduction number (Re)
#'
#' @param vc Vaccination coverage rate (proportion, 0 to 1).
#' @param disease Character string: "Measles", "Pertussis", "Chickenpox", or "Custom".
#' @param VE Vaccine effectiveness (proportion, 0 to 1).
#' @param r0_custom The basic reproduction number (R0) for a custom disease.
#'   Required only if `disease` is "Custom".
#' @return The effective reproduction number (Re).
#' @export
#' @examples
#' # For a standard disease
#' calc_re(vc = 0.92, disease = "Measles", VE = 0.97)
#' # For a custom disease like mumps (R0 approx. 10-12)
#' calc_re(vc = 0.88, disease = "Custom", VE = 0.85, r0_custom = 11)
calc_re <- function(vc, disease, VE, r0_custom = NULL) {
  r0 <- get_r0(disease, r0_custom)
  s <- 1 - (vc * VE)
  Re <- r0 * s
  return(Re)
}

#' Calculate the probability of at least one secondary infection
#'
#' @param Re The effective reproduction number.
#' @return The probability of at least one secondary infection.
#' @export
#' @examples
#' prob_sec_case(Re = 2.05)
prob_sec_case <- function(Re) {
  1 - exp(-Re)
}

#' Calculate the probability of a major outbreak
#'
#' @param Re The effective reproduction number.
#' @return The probability of a major outbreak (1 - 1/Re).
#' @export
#' @examples
#' prob_major_outbreak(Re = 2.05)
prob_major_outbreak <- function(Re) {
  ifelse(Re > 1, 1 - 1 / Re, 0)
}

#' Calculate the expected number of infections in a cohort
#'
#' @param vc Vaccination coverage rate (proportion, 0 to 1).
#' @param disease Character string: "Measles", "Pertussis", "Chickenpox", or "Custom".
#' @param kindergarten_size Integer, the size of the school cohort.
#' @param VE Vaccine effectiveness (proportion, 0 to 1).
#' @param r0_custom The basic reproduction number (R0) for a custom disease.
#'   Required only if `disease` is "Custom".
#' @return The expected number of infected individuals.
#' @export
#' @examples
#' calc_expected_infections(vc = 0.85, disease = "Measles", kindergarten_size = 200, VE = 0.97)
#' calc_expected_infections(
#'   vc = 0.90, disease = "Custom", kindergarten_size = 150, VE = 0.90, r0_custom = 7
#' )
calc_expected_infections <- function(vc, disease, kindergarten_size, VE, r0_custom = NULL) {
  s <- 1 - (vc * VE)
  r0 <- get_r0(disease, r0_custom)
  Re <- r0 * s
  final_size_eq <- function(z, Re) {
    1 - z - exp(-Re * z)
  }
  attack_rate <- if (Re > 1) {
    tryCatch(
      stats::uniroot(final_size_eq, interval = c(1e-9, 1 - 1e-9), Re = Re)$root,
      error = function(e) 0
    )
  } else {
    0
  }
  susceptible_count <- ceiling(kindergarten_size * s)
  expected_infections <- round(susceptible_count * attack_rate)
  return(expected_infections)
}
