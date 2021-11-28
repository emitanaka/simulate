

#' @export
simulate.data.frame <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  dots_nms <- names(dots)
  out <- .data
  for(i in seq_along(dots)) {
    arcrd <- dots_nms[i]
    res <- eval_tidy(dots[[i]], list(data = out, var = arcrd))
    out[[arcrd]] <- structure(res$values,
                              effects = res$effects)
  }
  out
}
