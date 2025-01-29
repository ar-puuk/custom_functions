#' Convert XML to HTML using XSLT
#'
#' This function applies an XSLT transformation to an XML file and saves the output as an HTML file.
#'
#' @param root A character string specifying the root directory where the XML and XSL files are located.
#' @param xml_filename A character string specifying the name of the XML file.
#' @param xsl_filename A character string specifying the name of the XSL file.
#' @param output_filename A character string specifying the name of the output HTML file.
#'
#' @return This function does not return a value but saves the transformed HTML file to the specified output path.
#'
#' @examples
#' xml_to_html("/path/to/files", "input.xml", "stylesheet.xsl", "output.html")
#'
#' @export
xml_to_html <- function(root, xml_filename, xsl_filename, output_filename) {
  # Construct file paths
  xml_path <- base::file.path(root, xml_filename)
  xsl_path <- base::file.path(root, xsl_filename)
  output_path <- base::file.path(root, output_filename)

  # Load the XML and XSLT files
  xml_doc <- xml2::read_xml(xml_path)
  xsl_doc <- xml2::read_xml(xsl_path)

  # Apply the XSLT transformation
  html_doc <- xslt::xml_xslt(xml_doc, xsl_doc)

  # Save the transformed HTML to the output file
  xml2::write_xml(html_doc, output_path)

  # Print success message
  base::cat("Transformation complete. HTML saved at:", output_path, "\n")
}
