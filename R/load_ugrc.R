#' @title Load List of Datasets from UGRC SGID
#'
#' @details
#' Retrieves a list of available datasets in the UGRC SGID database, including their schema and table names.
#'
#' @return A data frame containing two columns: `table_schema` and `table_name`, listing the schemas and tables in the SGID database.
#'
#' @examples
#' # Check list of available datasets
#' ugrc_vars <- load_ugrc_vars()
#' View(ugrc_vars)
#'
#' @export
load_ugrc_vars <- function() {
  # Establish connection to Open SGID database
  con <- dbConnect(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = "opensgid",
    host = "opensgid.agrc.utah.gov",
    port = 5432,
    user = "agrc",
    password = "agrc"
  )

  # SQL query to retrieve schema and table names
  query <- "
    SELECT table_schema, table_name
    FROM information_schema.tables
    WHERE table_type = 'BASE TABLE';
  "

  # Execute query and fetch results
  table_list <- tryCatch({
    dbGetQuery(con, query)
  }, error = function(e) {
    stop("Failed to retrieve table list: ", e$message)
  })

  # Safely disconnect from the database
  dbDisconnect(con)

  # Return the retrieved table list or handle errors if no tables are found
  if (nrow(table_list) == 0) {
    stop("No tables exist in the database.")
  }

  return(table_list)
}

#' @title Load Dataset from UGRC SGID
#'
#' @details
#' Loads a dataset from the UGRC SGID database. By default, the dataset is returned as an `sf` object for spatial data analysis.
#'
#' @param table_name A character string specifying the name of the table to retrieve (e.g., `"schema.table_name"`).
#' @param as_sf Logical. If `TRUE` (default), the data is returned as an `sf` object. If `FALSE`, the geometry column is dropped and a regular data frame is returned.
#'
#' @return An `sf` object or a data frame containing the data from the specified table.
#'
#' @examples
#' # Load UTA Transit Lines
#' uta_lines <- load_ugrc_data("transportation.uta_routes_and_ridership")
#'
#' # Load UTA Transit Stops
#' uta_stops <- load_ugrc_data("transportation.uta_stops_and_ridership")
#'
#' @export
load_ugrc_data <- function(table_name, as_sf = TRUE) {
  # Establish connection to Open SGID database
  con <- dbConnect(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = "opensgid",
    host = "opensgid.agrc.utah.gov",
    port = 5432,
    user = "agrc",
    password = "agrc"
  )

  # Construct SQL query to fetch data from the specified table
  query <- paste("SELECT * FROM", dbQuoteIdentifier(con, table_name))

  # Fetch the data as an sf object or handle errors
  df <- tryCatch({
    sf::read_sf(dsn = con, query = query, geometry_column = "shape") %>%
      sf::st_transform(3857) # Reproject to EPSG 3857
  }, error = function(e) {
    dbDisconnect(con) # Ensure the connection is closed on error
    stop("Failed to load spatial data: ", e$message)
  })

  # Safely disconnect from the database
  dbDisconnect(con)

  # Return the data as sf object or a regular data frame
  if (as_sf) {
    return(df)
  } else {
    return(tryCatch({
      sf::st_drop_geometry(df)
    }, error = function(e) {
      stop("Failed to drop geometry from sf object: ", e$message)
    }))
  }
}
