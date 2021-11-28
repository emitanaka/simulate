#' @export
params <- function(...) {
  structure(list2(...),
            class = "parameters")
}
