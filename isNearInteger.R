# This function will take a vector or scalar and test whether it is full of
# integer values or not.
# Since often integer values are stored in double data types it will test that
# a value is integer up to numerical precision (i.e., is near-integer).  In this
# case, you might need to as.integer the value in order to use them (e.g., for
# indexing).

isNearInteger = function(x, eps=1e-12){
  #flag for error checking
  error = FALSE

  #check to make sure that x is a vector of atomic types
  if(!is.atomic(x))
    stop("'x' must be an atomic vector.", call. = FALSE)

  #eps needs to be a scalar numeric value that is >= 0.
  if(!is.numeric(eps) || length(eps) > 1 || eps < 0)
    stop("'eps' must be a scalar numeric value >= 0.", call. = FALSE)

  ret = if(is.numeric(x)){
    if(is.integer(x) || all(abs(x - trunc(x)) < eps))
      TRUE else
        FALSE
  } else
    FALSE

  ret
}
