# dev_scripts/data_preparation.R

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(purrr)

# --- Read Data ---
raw <- readxl::read_excel("NonVitalInd_TenYrsReport.xlsx", col_names = FALSE)

# --- Data Cleaning (as provided in your analysis) ---
first_col <- raw[[1]] %>% as.character() %>% str_squish()
hdr_metric_row <- which(first_col == "County")[1]
hdr_year_row <- hdr_metric_row - 1
year_vals   <- raw[hdr_year_row, ] %>% as.character()
metric_vals <- raw[hdr_metric_row, ] %>% as.character()
year_vals <- zoo::na.locf(year_vals, na.rm = FALSE)

fill_left <- function(v){
  out <- v
  for (j in seq_along(out)) {
    if (is.na(out[j]) || out[j] == "") {
      if (j > 1 && !is.na(out[j-1]) && out[j-1] != "") out[j] <- out[j-1]
    }
  }
  out
}
year_vals <- fill_left(year_vals)
year_vals[1]   <- "County"
metric_vals[1] <- "County"

make_name <- function(j){
  if (j == 1) return("County")
  y <- str_squish(year_vals[j])
  m <- str_squish(metric_vals[j])
  if (is.na(y) || y == "") y <- if (j > 1) year_vals[j-1] else paste0("y", j)
  if (is.na(m) || m == "") m <- paste0("col", j)
  paste(y, m)
}
new_names <- purrr::map_chr(seq_len(ncol(raw)), make_name) %>% make.unique()

dat <- raw[(hdr_metric_row + 1):nrow(raw), , drop = FALSE]
names(dat) <- new_names

dat <- dat %>%
  mutate(County = str_squish(as.character(County))) %>%
  filter(!is.na(County), County != "", County != "Total")

percent_pat <- "^\\d{4}\\s+Percent\\s*\\(%\\)\\s*$"

florida_vaccine_coverage <- dat %>%
  select(County, matches(percent_pat)) %>%
  pivot_longer(-County, names_to = "YearLabel", values_to = "Coverage_Percent") %>%
  mutate(
    Year          = as.integer(sub(" .*", "", YearLabel)),
    Coverage_Rate = suppressWarnings(as.numeric(Coverage_Percent)) / 100
  ) %>%
  select(County, Year, Coverage_Rate) %>%
  filter(!is.na(Year), !is.na(Coverage_Rate))

# Save the data in the required format
usethis::use_data(florida_vaccine_coverage, overwrite = TRUE)
