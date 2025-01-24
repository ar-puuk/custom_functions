#' Convert KML to Simple Features (sf) Object with Preserved Attributes
#'
#' This function reads a KML file and processes the specified columns to extract
#' feature identifiers and descriptive attributes, returning an `sf` object
#' that retains the geometry and parsed attribute data.
#'
#' @param kml_path A character string specifying the path to the KML file. The path can be a local file or a URL, and zipped files are supported.
#' @param ... Additional arguments passed to `sf::read_sf()` for reading the KML file.
#' @param id_col A character string indicating the column in the KML file that contains feature identifiers (default is "Name").
#' @param details_col A character string specifying the column that contains descriptive data to be parsed (default is "Description").
#'
#' @return An `sf` object containing the geometry and parsed attributes extracted from the KML file.
#'
#' @details
#' The function parses the `details_col` to extract key-value pairs in the format "label: value",
#' ensuring that all features have a consistent structure. Any missing values are handled appropriately.
#'
#' @examples
#'
#' # Example usage:
#' # kml_file <- "path/to/your/file.kml"
#' # result_sf <- kml_to_sf(kml_file)
#'
#' @export
kml_to_sf <- function(kml_path, ..., id_col = "Name", details_col = "Description") {
  # Read KML into sf object
  v <- sf::st_read(kml_path, ...)

  # Ensure columns exist
  if (!(id_col %in% base::colnames(v))) stop(paste("ID column", id_col, "not found in KML file"))
  if (!(details_col %in% base::colnames(v))) stop(paste("Details column", details_col, "not found in KML file"))

  # Process the Description column (parse "label: value" pairs safely)
  dtext <- lapply(v[[details_col]], function(desc) {
    desc_cleaned <- base::gsub("<br>", "\n", desc)  # Standardize line breaks
    parsed <- strsplit(desc_cleaned, "\n")[[1]]  # Split into lines

    # Convert each "label: value" to named list
    attr_list <- lapply(parsed, function(line) {
      parts <- strsplit(line, ": ", fixed = TRUE)[[1]]
      if (length(parts) == 2) setNames(as.list(parts[2]), parts[1]) else NULL
    })

    # Combine all attributes into a single row
    data.frame(do.call(base::c, attr_list), stringsAsFactors = FALSE)
  })

  # Ensure consistent row binding by filling missing values
  sfx <- dplyr::bind_rows(dtext)

  # Combine the original ID and parsed attributes
  result <- base::cbind(sf::st_drop_geometry(v[, id_col, drop = FALSE]), sfx, geometry = sf::st_geometry(v))

  return(sf::st_as_sf(result))
}
