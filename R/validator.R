valid_prop <- function(x) {
  par <- deparse_to_char(enquo(x))
  if(!is_formula(x)) {
    if(any(x > 1 | x < 0)) abort(sprintf("The parameter value, %s, should be in [0,1].", par))
  }
}

valid_positive <- function(x) {
  par <- deparse_to_char(enquo(x))
  if(!is_formula(x)) {
    if(any(x < 0)) abort(sprintf("The parameter value, %s, should be positive.", par))
  }
}

valid_positive_or_null <- function(x) {
  par <- deparse_to_char(ensym(x))
  if(exists(par)) {
    if(is.null(x) && !is_formula(x)) {
      if(any(x < 0 & !is.null(x))) abort(sprintf("The parameter value, %s, should be positive.", par))
    }
  }
}

valid_integer <- function(x) {
  par <- deparse_to_char(ensym(x))
  if(!is_formula(x)) {
    if(any(x %% 1 != 0)) abort(sprintf("The parameter value, %s, should be integer.", par))
  }
}

valid_greater <- function(x, y) {
  parx <- deparse_to_char(enquo(x))
  pary <- deparse_to_char(ensym(y))
  if(!is_formula(x)) {
    if(any(y >= x)) abort(sprintf("The parameter value, %s, should be less than %s.", parx, pary))
  }
}
