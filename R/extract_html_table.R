#' Extract table following a specific anchor tag by name attribute
#'
#' This function extracts the summary and trip tables from Warren TDM HTML file.
#'
#' @param html_file A character string specifying the path to the HTML file.
#' @param anchor_id A character string specifying the `name` attribute of the anchor (`<a>`) tag to locate.
#'
#' @return A data frame containing the extracted table.
#'
#' @examples
#' \dontrun{
#' summary_table <- extract_html_table("Model.html", "id6")
#' # View the extracted table
#' View(summary_table[[1]])
#'
#' trip_table <- extract_html_table("Model.html", "id52")
#' # View the extracted table
#' View(trip_table[[1]])
#' }
#'
#' @export
extract_html_table <- function(html_file, anchor_id) {
  # Read the HTML content
  page <- rvest::read_html(html_file)

  # Find the anchor tag with the specified name attribute
  anchor_node <- page %>% rvest::html_node(xpath = base::paste0("//a[@name='", anchor_id, "']"))

  # Find the next table following the anchor tag
  if (!is.null(anchor_node)) {
    table <- anchor_node %>%
      rvest::html_elements(xpath = "following::table") %>%
      rvest::html_table()
  } else {
    stop("Anchor tag not found.")
  }

  # Return the extracted table as a data frame
  return(table)
}
