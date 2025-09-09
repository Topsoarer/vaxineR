#' Save Plot Data to a File
#'
#' An internal helper function to save the data frame underlying a plot to
#' either a CSV or an Excel file. When saving to Excel, it includes a
#' separate sheet for metadata.
#'
#' @param df The data frame to save.
#' @param path The file path where the data should be saved.
#' @param description A brief description of the data, used in the Excel metadata.
#' @return Invisibly returns the input data frame.
#' @importFrom tools file_ext
#' @importFrom utils write.csv
#' @noRd
save_plot_data <- function(df, path, description) {
  # Check if the path is valid
  if (is.null(path) || !is.character(path) || length(path) != 1) {
    stop("'save_data_to' path must be a single string.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))

  if (ext == "xlsx") {
    # Check if openxlsx is installed
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required to save data to .xlsx files. Please install it.", call. = FALSE)
    }

    # Create metadata
    meta_df <- data.frame(
      Field = c("Description", "Source Package", "Data Saved On"),
      Value = c(description, "vaxineR", as.character(Sys.Date()))
    )

    # Create a workbook and add data and metadata sheets
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Data")
    openxlsx::addWorksheet(wb, "Metadata")
    openxlsx::writeData(wb, "Data", df)
    openxlsx::writeData(wb, "Metadata", meta_df)
    openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)

    message("Plot data and metadata saved to '", path, "'")

  } else if (ext == "csv") {
    utils::write.csv(df, path, row.names = FALSE)
    message("Plot data saved to '", path, "'")

  } else {
    stop("Unsupported file extension: '", ext, "'. Please use '.csv' or '.xlsx'.", call. = FALSE)
  }

  invisible(df)
}
