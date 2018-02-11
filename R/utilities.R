
#' Resolves quosures so that character input given to a quoted argument is resolved to symbol.
#'
#' Adapted from https://stackoverflow.com/q/43700765
#'
#' @param x A quoted variable that needs to be resolved
#' @return The resolved quosure
#'
#' @importFrom rlang is_empty get_env UQE
#' @importFrom dplyr quo_name
#' @export
resolve_quosure <- function(x){
  #if(!length(tryCatch({ls(get_env(x))}, error=function(e) "empty")))
  if (is_empty(get_env(x))) {
    if (!is.null(UQE(x))) {
      x <- as.name(quo_name(x))
    } else {
      x <- NULL
    }
  }

  x
}
