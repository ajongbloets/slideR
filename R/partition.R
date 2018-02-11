#' Calculate the boundaries of a window, given a position and alignment.
#' In a centered window the lower and upper bound are at equal distance from the index.
#' Unless the windows is of even size, then the upper bound is 1 position closer to the center.
#'
#' @param idx Index of window key
#' @param w_size Size of window in points
#' @param align Alignment of window, relative to idx.
#'    "center" means: idx is in the middle between lower and upper bound.
#'    "left" means: idx is on the left, so idx == lower bound.
#'    "right" means: idx is on the right, so idx == upper bound.
#' @return A vector of length 2 containing the lower and upper bound.
#'
#' @export
window_boundaries <- function( idx, w_size, align="center" ) {
  lwr <- NA
  upr <- NA
  if (align == "center") {
    # center = idx == middle
    lwr <- idx - w_size %/% 2 + 1
    upr <- idx + w_size %/% 2 + w_size %% 2
  } else if (align == "left") {
    # left align = idx == lwr
    lwr <- idx
    upr <- idx + w_size - 1
  } else if (align == "right") {
    # right align = idx == upr
    lwr <- idx - w_size + 1
    upr <- idx
  }
  return(c(lwr, upr))
}

#' Create a window of certain size at each index of the data.
#'
#' @param idx The index of the window key.
#' @param w_size Size of window in points.
#' @param keys The values used to create and identify the windows.
#' @param data The data that will be filtered and divided into windows.
#' @param partial Whether a partial window is allowed.
#'  At the begin and end of a dataset, some windows will be smaller than w_size.
#'  These windows will be removed is partial is FALSE.
#' @param align Alignment of the window, relative to idx.
#'    "center" means: idx is in the middle between lower and upper bound.
#'    "left" means: idx is on the left, so idx == lower bound.
#'    "right" means: idx is on the right, so idx == upper bound.
#' @return The subset of keys (or data) that are in the window
#' @importFrom dplyr filter row_number mutate
#' @export
partition_window <- function(
  idx, w_size, keys, data=NULL, partial=T, align="center"
) {
  result <- data.frame()
  w.bounds <- window_boundaries(idx, w_size, align)
  # if data is given, use data as values otherwise use the keys as data
  values <- (if(!is.null(data)) data else keys)
  v.size <- (if(is.data.frame(values)) nrow(values) else length(values))
  # reset boundaries based on the data availability
  w.bounds[[1]] <- (if(w.bounds[[1]] < 1) 1 else w.bounds[[1]])
  w.bounds[[2]] <- (if(w.bounds[[2]] > v.size) v.size else w.bounds[[2]])
  # calculate actual window size
  w.size <- w.bounds[[2]] - w.bounds[[1]] + 1
  # only proceed if the window contains data, partial is accepted or
  # the actual window size equals the desired window size
  if ( w.size > 0 && (partial || w.size == w_size)) {
    # treat data frames differently
    if (is.data.frame(values)) {
      # filter by bounds (not using slice, to be compatible with rel. dbs)
      result <- values %>%
        filter(row_number() >= w.bounds[[1]], row_number() <= w.bounds[[2]])
    } else {
      result <- values[w.bounds[[1]]:w.bounds[[2]]]
    }
  }
  return(result)
}
