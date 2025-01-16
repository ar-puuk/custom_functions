#' Source: https://gist.github.com/dblodgett-usgs/cf87392c02d73f1b7d16153d2b66a8f3
#'
#' @title split lines
#' @description Splits lines longer than a given threshold into the minimum number of pieces to all be under the given threshold.
#' @param lines data.frame of class sf with LINESTRING sfc column.
#' @param max_length maximum segment length to return
#' @param id name of ID column in data.frame
#' @return only the split lines.
#' @importFrom dplyr group_by ungroup filter left_join select rename mutate
#' @export
#'
#' @examples
#' library(sf)
#' library(dplyr)
#'
#' geojson <- '{
#' "type": "FeatureCollection",
#' "name": "sample",
#' "features": [
#'   { "type": "Feature", "properties": { "COMID": 5329303 }, "geometry": { "type": "LineString", "coordinates": [ [ -122.916097119649635, 38.229575873993497 ], [ -122.916154986316201, 38.229346873993904 ], [ -122.916676986315338, 38.228614207328235 ], [ -122.917430786314128, 38.227148873997294 ], [ -122.917488319647418, 38.226210273998674 ], [ -122.917371986314322, 38.22579827399926 ], [ -122.917400319647584, 38.224516407334704 ], [ -122.918995719645125, 38.223348274003115 ], [ -122.920127119643382, 38.223004607336975 ], [ -122.921171719641791, 38.222546407337802 ], [ -122.922186919640126, 38.221950807338601 ], [ -122.922795786305926, 38.221286674006421 ] ] } },
#'   { "type": "Feature", "properties": { "COMID": 5329293 }, "geometry": { "type": "LineString", "coordinates": [ [ -122.913574186320261, 38.233262207321104 ], [ -122.914618986318601, 38.233261873987772 ], [ -122.915228386317608, 38.23307847398803 ], [ -122.915547519650431, 38.23282660732184 ], [ -122.915866386316679, 38.232231273989385 ], [ -122.915778986316809, 38.231475873990462 ], [ -122.915430386317382, 38.230880807324809 ], [ -122.915400986317422, 38.23044600732544 ], [ -122.91548798631726, 38.23024000732579 ], [ -122.916097119649635, 38.229575873993497 ] ] } },
#'   { "type": "Feature", "properties": { "COMID": 5329305 }, "geometry": { "type": "LineString", "coordinates": [ [ -122.895114186348906, 38.228161607328957 ], [ -122.895375386348462, 38.22809287399582 ], [ -122.895636519681375, 38.227864007329401 ], [ -122.895897586347701, 38.227337407330253 ], [ -122.89607138634733, 38.226559073998033 ], [ -122.896245519680463, 38.226330073998497 ], [ -122.896071186347342, 38.225597607333043 ], [ -122.896186986347232, 38.224750474000984 ], [ -122.896070786347309, 38.224407207334878 ], [ -122.896041519680807, 38.223354207336399 ], [ -122.895867319681031, 38.223079474003612 ], [ -122.895867186347687, 38.222598874004348 ], [ -122.896070186347345, 38.222324207338033 ], [ -122.896940719679321, 38.221843274005437 ], [ -122.897492119678532, 38.221934674005354 ], [ -122.897985519677775, 38.222323807338 ], [ -122.898565986343442, 38.222529674004477 ], [ -122.89937871967561, 38.223056074003637 ], [ -122.900336386340712, 38.2235366073362 ], [ -122.90106218633963, 38.224406274001467 ], [ -122.901323386339243, 38.224566474001222 ], [ -122.901903719671566, 38.224497674001384 ], [ -122.902164786337892, 38.22429160733509 ], [ -122.902193586337887, 38.223673607336025 ], [ -122.902048519671439, 38.223444674003076 ], [ -122.901438919672387, 38.222941207337044 ], [ -122.901380786339189, 38.222597807337593 ], [ -122.901119519672761, 38.222300207338037 ], [ -122.901119386339587, 38.221819607338773 ], [ -122.901873386338366, 38.220743607340466 ], [ -122.902279586337784, 38.220651874007388 ], [ -122.902714986337116, 38.220651807340687 ], [ -122.902976186336616, 38.220766207340432 ], [ -122.903237519669631, 38.221338474006302 ], [ -122.903556919669086, 38.221704607339007 ], [ -122.904195386334777, 38.222139407338375 ], [ -122.905704586332433, 38.222574007337755 ], [ -122.906662319664292, 38.222940007337115 ], [ -122.908577919661298, 38.223878074002357 ], [ -122.910203519658751, 38.224564407334583 ], [ -122.910580919658173, 38.224999207333951 ], [ -122.910552119658121, 38.225457073999792 ], [ -122.910378119658446, 38.225754673999347 ], [ -122.909623786326392, 38.22628140733184 ], [ -122.908927586327422, 38.227037007330807 ], [ -122.908289519661764, 38.22751787399659 ], [ -122.907767519662571, 38.228456607328496 ], [ -122.90782578632917, 38.22902900732754 ], [ -122.908087186328771, 38.229463673993621 ], [ -122.908377519661599, 38.229738273993235 ], [ -122.909451386326623, 38.230012873992848 ], [ -122.9101481196588, 38.230333073992369 ], [ -122.910409386325057, 38.230653473991879 ], [ -122.910438519658385, 38.230836607324875 ], [ -122.910903119657689, 38.231271473990773 ], [ -122.911367519657006, 38.231591807323582 ], [ -122.911454786323532, 38.231958007323158 ], [ -122.911832319656298, 38.23234707398916 ], [ -122.912557986321815, 38.232667273988682 ], [ -122.913574186320261, 38.233262207321104 ] ] } }
#' ]
#' }'
#'
#' lines <- sf::st_transform(sf::st_read(geojson), 5070)
#'
#' split_lines <- st_split_lines(lines, 500, id = "COMID")
#' plot(split_lines)
#'
st_split_lines <- function(input_lines, max_length, id = "ID") {
  geom_column <- attr(input_lines, "sf_column")

  input_crs <- sf::st_crs(input_lines)

  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- filter(select(input_lines, id, geom_column, geom_len), geom_len >= max_length)

  rm(input_lines) # just to control memory usage in case this is big.

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_len / max_length),
                     piece_len = (geom_len / pieces),
                     fID = 1:nrow(too_long))

  split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),]

  split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
    select(-geom_len, -pieces) %>%
    group_by(fID) %>%
    mutate(ideal_len = cumsum(piece_len)) %>%
    ungroup()

  coords <- data.frame(sf::st_coordinates(too_long[[geom_column]]))
  rm(too_long)

  coords <- rename(coords, fID = L1) %>% mutate(nID = 1:nrow(coords))

  split_nodes <- group_by(coords, fID) %>%
    # First calculate cumulative length by feature.
    mutate(len  = sqrt(((X - (lag(X)))^2) + (((Y - (lag(Y)))^2)))) %>%
    mutate(len = ifelse(is.na(len), 0, len)) %>%
    mutate(len = cumsum(len)) %>%
    # Now join nodes to split points -- this generates all combinations.
    left_join(select(split_points, fID, ideal_len, split_fID), by = "fID") %>%
    # Calculate the difference between node-wise distance and split-point distance.
    mutate(diff_len = abs(len - ideal_len)) %>%
    # regroup by the new split features.
    group_by(split_fID) %>%
    # filter out na then grab the min distance
    filter(!is.na(diff_len) & diff_len == min(diff_len)) %>%
    ungroup() %>%
    # Grab the start node for each geometry -- the end node of the geometry before it.
    mutate(start_nID = lag(nID),
           # need to move the start node one for new features.
           new_feature = fID - lag(fID, default = -1),
           start_nID = ifelse(new_feature == 1, start_nID + 1, start_nID)) %>%
    # Clean up the mess
    select(fID, split_fID, start_nID, stop_nID = nID, -diff_len, -ideal_len, -len, -X, -Y)

  split_nodes$start_nID[1] <- 1

  split_points <- left_join(split_points, select(split_nodes, split_fID, start_nID, stop_nID), by = "split_fID")

  new_line <- function(start_stop, coords) {
    sf::st_linestring(as.matrix(coords[start_stop[1]:start_stop[2], c("X", "Y")]))
  }

  split_lines <- apply(as.matrix(split_points[c("start_nID", "stop_nID")]),
                       MARGIN = 1, FUN = new_line, coords = coords)

  split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))

  return(split_lines)
}

###################################################################################################
# Second version of this function using lwgeom per: https://github.com/r-spatial/lwgeom/issues/16 #
###################################################################################################

#' @title split lines
#' @description Splits lines longer than a given threshold into the minimum number of pieces to all be under the given threshold.
#' @param lines data.frame of class sf with LINESTRING sfc column.
#' @param max_length maximum segment length to return
#' @param id name of ID column in data.frame
#' @return only the split lines.
#' @importFrom dplyr group_by ungroup filter select mutate
#' @export
#'
#' @examples
#' library(sf)
#' library(dplyr)
#'
#' geojson <- '{
#' "type": "FeatureCollection",
#' "name": "sample",
#' "features": [
#'   { "type": "Feature", "properties": { "COMID": 5329303 }, "geometry": { "type": "LineString", "coordinates": [ [ -122.916097119649635, 38.229575873993497 ], [ -122.916154986316201, 38.229346873993904 ], [ -122.916676986315338, 38.228614207328235 ], [ -122.917430786314128, 38.227148873997294 ], [ -122.917488319647418, 38.226210273998674 ], [ -122.917371986314322, 38.22579827399926 ], [ -122.917400319647584, 38.224516407334704 ], [ -122.918995719645125, 38.223348274003115 ], [ -122.920127119643382, 38.223004607336975 ], [ -122.921171719641791, 38.222546407337802 ], [ -122.922186919640126, 38.221950807338601 ], [ -122.922795786305926, 38.221286674006421 ] ] } },
#'   { "type": "Feature", "properties": { "COMID": 5329293 }, "geometry": { "type": "LineString", "coordinates": [ [ -122.913574186320261, 38.233262207321104 ], [ -122.914618986318601, 38.233261873987772 ], [ -122.915228386317608, 38.23307847398803 ], [ -122.915547519650431, 38.23282660732184 ], [ -122.915866386316679, 38.232231273989385 ], [ -122.915778986316809, 38.231475873990462 ], [ -122.915430386317382, 38.230880807324809 ], [ -122.915400986317422, 38.23044600732544 ], [ -122.91548798631726, 38.23024000732579 ], [ -122.916097119649635, 38.229575873993497 ] ] } },
#'   { "type": "Feature", "properties": { "COMID": 5329305 }, "geometry": { "type": "LineString", "coordinates": [ [ -122.895114186348906, 38.228161607328957 ], [ -122.895375386348462, 38.22809287399582 ], [ -122.895636519681375, 38.227864007329401 ], [ -122.895897586347701, 38.227337407330253 ], [ -122.89607138634733, 38.226559073998033 ], [ -122.896245519680463, 38.226330073998497 ], [ -122.896071186347342, 38.225597607333043 ], [ -122.896186986347232, 38.224750474000984 ], [ -122.896070786347309, 38.224407207334878 ], [ -122.896041519680807, 38.223354207336399 ], [ -122.895867319681031, 38.223079474003612 ], [ -122.895867186347687, 38.222598874004348 ], [ -122.896070186347345, 38.222324207338033 ], [ -122.896940719679321, 38.221843274005437 ], [ -122.897492119678532, 38.221934674005354 ], [ -122.897985519677775, 38.222323807338 ], [ -122.898565986343442, 38.222529674004477 ], [ -122.89937871967561, 38.223056074003637 ], [ -122.900336386340712, 38.2235366073362 ], [ -122.90106218633963, 38.224406274001467 ], [ -122.901323386339243, 38.224566474001222 ], [ -122.901903719671566, 38.224497674001384 ], [ -122.902164786337892, 38.22429160733509 ], [ -122.902193586337887, 38.223673607336025 ], [ -122.902048519671439, 38.223444674003076 ], [ -122.901438919672387, 38.222941207337044 ], [ -122.901380786339189, 38.222597807337593 ], [ -122.901119519672761, 38.222300207338037 ], [ -122.901119386339587, 38.221819607338773 ], [ -122.901873386338366, 38.220743607340466 ], [ -122.902279586337784, 38.220651874007388 ], [ -122.902714986337116, 38.220651807340687 ], [ -122.902976186336616, 38.220766207340432 ], [ -122.903237519669631, 38.221338474006302 ], [ -122.903556919669086, 38.221704607339007 ], [ -122.904195386334777, 38.222139407338375 ], [ -122.905704586332433, 38.222574007337755 ], [ -122.906662319664292, 38.222940007337115 ], [ -122.908577919661298, 38.223878074002357 ], [ -122.910203519658751, 38.224564407334583 ], [ -122.910580919658173, 38.224999207333951 ], [ -122.910552119658121, 38.225457073999792 ], [ -122.910378119658446, 38.225754673999347 ], [ -122.909623786326392, 38.22628140733184 ], [ -122.908927586327422, 38.227037007330807 ], [ -122.908289519661764, 38.22751787399659 ], [ -122.907767519662571, 38.228456607328496 ], [ -122.90782578632917, 38.22902900732754 ], [ -122.908087186328771, 38.229463673993621 ], [ -122.908377519661599, 38.229738273993235 ], [ -122.909451386326623, 38.230012873992848 ], [ -122.9101481196588, 38.230333073992369 ], [ -122.910409386325057, 38.230653473991879 ], [ -122.910438519658385, 38.230836607324875 ], [ -122.910903119657689, 38.231271473990773 ], [ -122.911367519657006, 38.231591807323582 ], [ -122.911454786323532, 38.231958007323158 ], [ -122.911832319656298, 38.23234707398916 ], [ -122.912557986321815, 38.232667273988682 ], [ -122.913574186320261, 38.233262207321104 ] ] } }
#' ]
#' }'
#'
#' lines <- sf::st_transform(sf::st_read(geojson), 5070)
#'
#' split_lines <- geo_split_lines(lines, 500, id = "COMID")
#' plot(split_lines)
#'
geo_split_lines <- function(input_lines, max_length, id = "ID") {
  if(max_length < 50) warning("short max length detected, do you have your units right?")

  geom_column <- attr(input_lines, "sf_column")

  input_crs <- sf::st_crs(input_lines)

  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- filter(select(input_lines, id, geom_column, geom_len), geom_len >= max_length)

  rm(input_lines) # just to control memory usage in case this is big.

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_len / max_length),
                     fID = 1:nrow(too_long)) %>%
    select(-geom_len)

  split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),] %>%
    select(-pieces)

  split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
    group_by(fID) %>%
    mutate(piece = 1:n()) %>%
    mutate(start = (piece - 1) / n(),
           end = piece / n()) %>%
    ungroup()

  new_line <- function(i, f, t) {
    lwgeom::st_linesubstring(x = too_long[[geom_column]][i], from = f, to = t)[[1]]
  }

  split_lines <- apply(split_points[c("fID", "start", "end")], 1,
                       function(x) new_line(i = x[["fID"]], f = x[["start"]], t = x[["end"]]))

  rm(too_long)

  split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))

  return(split_lines)
}


