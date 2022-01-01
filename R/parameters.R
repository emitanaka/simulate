#' Set the parameters in a simulation framework
#'
#' @param x A simulation framework
#' @param param A character vector with the name of the parameter to modify or set.
#' @param ... A named list of values to set.
#' @param .reset To reset the values of the parameters.
#' @export
params <- function(.x, .param = NULL, ..., .reset = FALSE) {
  args <- attr(.x, "args")
  prms <- list2(...)
  #browser()
  if(is.null(.param)) {
    args$params <- prm_to_lower_level(c(args$params, prms), names(args$input))
  } else {
    for(aparam in .param) {
      for(aparam_inner in names(prms)) {
        args$params[[aparam]][[aparam_inner]] <- prms[[aparam_inner]]
      }
    }
  }
  attr(.x, "args") <- args
  .x
}
