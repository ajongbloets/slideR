#' Calculate the boundaries of a window, given a position and alignment.
#'
#' @param idx Index of window key
#' @param w_size Size of window in points
#' @param align Alignment of window, relative to idx.
#'    "center" means: idx is in the middle between lower and upper bound.
#'    "left" means: idx is on the left, so idx == lower bound.
#'    "right" means: idx is on the right, so idx == upper bound.
#' @return A vector of length 2 containing the lower and upper bound.
#' @export
window_bounds <- function( idx, w_size, align="center" ) {
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
#' @return A data.frame with all values within the window.
#' @importFrom dplyr filter row_number mutate
#' @export
create_window <- function(
  idx, w_size, keys, data=NULL, partial=T, align="center", ...
) {
  result <- data.frame()
  w.bounds <- window_bounds(idx, w_size, align)
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
        filter(
          row_number() >= w.bounds[[1]],
          row_number() <= w.bounds[[2]]
        )
      # add key column
      result <- mutate(result, key = keys[[idx]])
    } else {
      result <- values[w.bounds[[1]]:w.bounds[[2]]]
      result <- data.frame(data=result, key=keys[[idx]])
    }
  }
  return(result)
}

#' Create windows of data, using keys in column key
#'
#' @param data A data.frame or list of data points
#' @param key Name of column in data (if data is data.frame) that should be used
#' as key for each window.
#' @param fun Function that should be applied to each window
#' @param ... Arguments that should be passed to the create_window function
#' @return A data.frame with all windows along with corresponding key.
#' @importFrom dplyr select_ arrange_
#' @export
create_windows <- function(
  data, key=NULL, w_size=10, ...
) {
  # stop if data is not a list and key is null
  stopifnot( !is.data.frame(data) || !is.null(key))
  # get the number of points in the source
  k.size <- ifelse(is.data.frame(data), nrow(data), length(data))
  # get the keys used to identify each window
  keys <- data
  # select the key column from the data if a key column is given
  if (is.data.frame(data) && !is.null(keys)) {
    keys <- unlist(data %>% arrange_(key) %>% select_(key))
  }
  # Create a window for each key and bind them into one data.frame
  return(
    map(
      seq_along(keys),
      ~create_window(., keys=keys, data=data, w_size=w_size, ...)
    ) %>% bind_rows()
  )
}

#' Creates sliding windows from the data and returns them as a nested data.frame
#'
#' Basically a wrapper around create_windows and nest
#'
#' @param data A vector, list or data.frame
#' @param key If data is data.frame,
#' provide a column name that serves as key for each window
#' @param w_size The size of the windows
#' @return A nested data.frame with two columns: "key" and "data"
#' @examples
#' df.test <- data.frame( x=1:100, y=1:100*2 )
#' nest_windows(df.test, key="x")
#' @importFrom dplyr group_by
#' @importFrom tidyr nest
#' @export
nest_windows <- function(data, key=NULL, w_size=10, ...) {
  return(
    create_windows(data, key=key, w_size=w_size, ...) %>%
      group_by(key) %>%
      nest()
  )
}
