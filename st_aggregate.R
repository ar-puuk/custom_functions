#' Aggregate sf objects
#' Source: https://gist.github.com/rCarto/bb47aff0a02e808d2bf64f2d8c5db7d8#file-st_aggregate-r
#' Geometries and attributes are aggregated.
#'
#' @param x sf object
#' @param by name of the variable of grouping elements
#' @param var name(s) of the variable(s) to aggregate
#' @param fun function(s) to compute the summary statistics
#'
#' @return An sf object is returned
#' @export
#'
#' @examples
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc$dummy <- "ZONE_A"
#' nc$dummy[25:50] <- "ZONE_B"
#' nc$dummy[51:100] <- "ZONE_C"
#' r <- st_aggregate(nc, "dummy", c("BIR74", "NWBIR74"), c("mean", "median"))
#' plot(nc)
#' plot(r)
#'
st_aggregate <- function(x, by, var, fun){
  var = c(by, var)
  fun = c("head", fun)
  n <- length(var)
  l <- vector("list", n)
  for (i in 1:n){
    l[[i]] <- tapply(x[[var[i]]], x[[by]], add_args(fun[[i]]), n = 1, na.rm = TRUE)
  }
  names(l) <- var
  r <- sf::st_sf(do.call(data.frame, l),
                 geometry = tapply(x[attr(x, "sf_column")], x[[by]], sf::st_union),
                 crs = sf::st_crs(x))
  r
}

add_args <- function(x){
  rx <- get(x)
  fx <- formals(rx)
  fx$na.rm = TRUE
  fx$n = 1
  formals(rx) <- fx
  rx
}
