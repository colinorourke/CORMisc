repchar = function(x,times=1L){
  #I want to rule out lists and things
  if(!is.vector(x))
    stop("'x' must be a scalar value (", typeof(x), " supplied).", call. = FALSE)

  #I want only a single value, so if >1, keep only first with
  #warning
  x = if(length(x) > 1){
    warning("Length of 'x' > 1, only first value used", call. = FALSE)
    x[1]
  } else
    x

  #check that input is a character value.  If not, convert
  #to character with warning
  if(!is.character(x)){
    warning(typeof(x), " 'x' converted to character value.", call. = FALSE)
    x = as.character(x)
  }

  #times has to be numeric
  if(!(is.vector(times) && is.numeric(times)))
    stop("'times' must be numeric.", call. = FALSE)

  #times must be an integer
  times = if(is.integer(times))
    times else {
      if(all(abs(times - trunc(times)) < 1e-12))
        as.integer(times) else
          stop("Value of 'times' must be integer.", call. = FALSE)
    }

  #times must be >= 0
  if(any(times < 0))
    stop("'times' must be >= 0.", call. = FALSE)

  #return the proper value
  #if times is a vector, make sure that we get
  #a vector of repetitions back out.
  if(length(times) == 1){
    if(times == 0)
      "" else if (times == 1)
        x else
          paste(rep(x,times),collapse="")
  } else
    vapply(times,
           FUN=function(times,x) {
             if(times == 0)
               "" else if (times == 1)
                 x else
                   paste(rep(x,times),collapse="")
           },
           x=x,
           FUN.VALUE = character(1)
    )
}
