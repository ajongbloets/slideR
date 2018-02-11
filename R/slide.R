#' Partition data using a sliding window
#'
#' @param data A data.frame or list of data points
#' @param key Name of column in data (if data is data.frame) that should be used
#'  as key for each window.
#' @param w_size The size of the window to create
#' @param ... Arguments that should be passed to the create_window function
#' @return A nested data.frame with all windows along with corresponding key.
#'
#' @importFrom dplyr select arrange rename
#' @importFrom purrr partial map_dbl
#' @export
slide_window <- function(data, key=NULL, w_size=10, ...) {
  key <- enquo(key) %>% resolve_quosure()

  # stop if data is a data.frame and key is null
  stopifnot( !is.data.frame(data) || !is.null(key))

  # get the number of points in the source
  k.size <- ifelse(is.data.frame(data), nrow(data), length(data))
  # get the keys used to identify each window
  keys <- data
  # select the key column from the data if a key column is given
  if (is.data.frame(data)) {
    stopifnot(quo_name(key) %in% colnames(data))

    keys <- unlist(data %>% arrange(UQ(key)) %>% select(UQ(key)))
  }
  # prefill create window with variables
  f <- partial(
    partition_window, keys=keys, data=data, w_size=w_size, ..., .lazy=F
  )
  # create sliding windows as a nested dataset and calculate window sizes
  result <- data.frame( key = keys) %>%
    mutate(
      data = map(row_number(key), f),
      w_size = map_dbl(data, ~ifelse(is.data.frame(.), nrow(.), length(.)))
    ) %>%
    filter(w_size > 0)

  if (!is.null(key)) {
    key_name <- quo_name(key)

    result <- result %>%
      rename(UQ(key_name) := key)
  }

  return(result)
}

#' Partition data using sliding windows of different sizes
#'
#' @param data A data.frame or list of data points to partition
#' @param key Column name to use a key when partitioning,
#'  used only when data is data.frame
#' @param w_sizes Sizes of the windows to create
#' @param ... Arguments that should be passed to the slide_window function
#' @return A nested data.frame with all windows of all sizes,
#'  with `key` column containing windows keys and `w_size` with window size.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
slide_windows <- function(data, key=NULL, w_sizes=10, ...) {

  key <- enquo(key)
  w_sizes <- unique(w_sizes)

  result <- map(
      w_sizes, slide_window, data=data, key=UQ(key), ...
  ) %>%
    bind_rows()

  return(result)
}

#' Apply a function to each window in a sliding window
#'
#' @param data A data.frame or list of data points to partition
#' @param f function to apply to each window
#' @param key Column name to use a key when partitioning,
#'  used only when data is data.frame
#' @param w_size Size of the sliding window
#' @param .to Name of the column to store the function values in
#' @param .keep_data whether to keep the partitioned data (defaults to TRUE)
#' @param ... Arguments that should be passed to the slide_window function
#' @return A nested data.frame with data partitioned in the data column,
#'  a `key` column containing windows keys and `w_size` with window size
#'  and a column named after f or the value specified in .to containing the
#'  value using the function f
#'
#' @importFrom dplyr mutate_ select
#' @importFrom purrr is_formula
#' @importFrom stats setNames
#' @export
apply_slide_window <- function(
  data, f, key=NULL, w_size=10, .to=".out", .keep_data=T, ...
) {

  stopifnot(is.function(f) || is_formula(f))
  key <- enquo(key)
  .to <- enquo(.to) %>% resolve_quosure()

  if (is.null(.to)) {
    .to <- as.character(quote(f))
  }

  result <- slide_window(data, key=UQ(key), w_size=w_size, ...) %>%
    mutate(UQ(quo_name(.to)) := map(data, f))

  if (!.keep_data) {
    result <- result %>% select(-data)
  }

  return(result)
}

#' Apply a function to each window in sliding windows of different sizes.
#'
#' @param data A data.frame or list of data points to partition
#' @param f function to apply to each window
#' @param key Column name to use a key when partitioning,
#'  used only when data is data.frame
#' @param w_sizes Sizes of the sliding windows
#' @param .to Name of the column to store the function values in
#' @param .keep_data whether to keep the partitioned data (defaults to TRUE)
#' @param ... Arguments that should be passed to the slide_window function
#' @return A nested data.frame with data partitioned in the data column,
#'  a `key` column containing windows keys and `w_size` with window size
#'  and a column named after f or the value specified in .to containing the
#'  value using the function f
#'
#' @importFrom purrr map
#' @export
apply_slide_windows <- function(
  data, f, key=NULL, w_sizes=10, .to=".out", .keep_data=T, ...
) {

  key <- enquo(key)
  .to <- enquo(.to)
  w_sizes <- unique(w_sizes)

  result <- map(
    w_sizes, ~apply_slide_window(
      data, f=f, key=UQ(key), w_size=.x, .to=UQ(.to), .keep_data=.keep_data, ...
    )
  ) %>%
    bind_rows()

  return(result)
}
