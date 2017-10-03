fmtP = function(pval, strict=TRUE){
  #pval must be supplied!
  if(missing(pval))
    stop("'pval' argument is required.")

  #strict must be logical scalar.
  if(!is.logical(strict) || length(strict) > 1)
    stop("'strict' must be logical scalar value.")

  #pval must be numeric vector
  if(!is.numeric(pval)) stop("'pval' must be numeric.")

  #which are missing?
  pval_is_na = is.na(pval)

  #just return pval (vector of NAs) if all are missing
  if(all(pval_is_na)) {
    warning("All 'pval' are NA.")
    return(pval)
  }

  #if strict is TRUE force 0 <= pval[i] <= 1, for all i in 1 to length(pval).
  #otherwise, just issue warning.  Not sure why one'd want the strict
  #option, but maybe useful in certain situations where you don't want
  #code to error out?
  #only check non-missing values
  if(any(pval[!pval_is_na] < 0) || any(pval[!pval_is_na] > 1)){
    if(strict)
      stop("All 'pval' must be in interval [0,1].") else
        warning("All 'pval' should in in interval [0,1].")
  }


  ret = vapply(
    pval,
    FUN = function(x){
      x_rnd = round(x,3)
      if(x_rnd <= 0)
        "< 0.001" else
          if(x_rnd < 0.1)
            sprintf("%.3f",x_rnd) else
              if(x_rnd < 0.99)
                sprintf("%0.2f",x_rnd) else
                  "> 0.99"
    },
    FUN.VALUE = character(1))

  ret
}
