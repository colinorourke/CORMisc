# This function takes 3 required arguments:
#
#  x (a numeric vector, the variable you want to use
#     to apply the spline expansion on)
#  t (the sequence of knots)
#  i (which individual spline variable you're interested
#     in)
#
#  It also takes 2 additional optional arguments:
#
#   scale (this scales the spline variables by the square
#         of the differences between the first and last
#         knots)
#   deriv (if you want to return the derivative instead of
#          the original spline variables)

rcs.x = function(x,t,i,scale=TRUE, deriv=FALSE){
  if(!is.numeric(x) || !length(x))
    stop("'x' must be a vector of numeric values.", call. = FALSE)
  k=length(t)
  if(!is.numeric(t))
    stop("'t' must be a vector of numeric values.", call. = FALSE)
  if(!is.numeric(i) || !length(i) || any(abs(i - trunc(i)) > 1e-12))
    stop("'i' must be an integer value.", call. = FALSE)
  if(length(i) > 1){
    i = i[1]
    warning("Length of 'i' > 1.  Only first element used (i = ", i, ").", call. = FALSE)
  }
  if(!is.integer(i)){
    i = as.integer(i)
  }
  if(k < 3)
    stop("'t' must contain at least one interior knot.", call. = FALSE)
  if(is.unsorted(t, strictly = TRUE))
    stop("'t' must be such that t[i] < t[j], for all i < j.", call. = FALSE)
  if(i < 1 || i > k-2)
    stop("'i' must be at least 1 but less than ", k-1, call. = FALSE)
  if(!is.logical(scale) || !length(scale))
    stop("'scale' must be logical value")
  if(!is.logical(deriv) || !length(deriv))
    stop("'deriv' must be logical value")
  if(length(scale) > 1){
    scale = scale[1]
    warning("Length of 'scale' > 1.  Only first element used (scale = ", scale, ").", call. = FALSE)
  }
  if(length(deriv) > 1){
    deriv = deriv[1]
    warning("Length of 'deriv' > 1.  Only first element used (deriv = ", deriv, ").", call. = FALSE)
  }

  p0 = function(x) return(pmax(x,0))
  f1 = (t[k]-t[i])/(t[k]-t[k-1])
  f2 = (t[k-1]-t[i])/(t[k]-t[k-1])
  ret = if(deriv)
    3 * p0(x-t[i])^2 - 3*p0(x-t[k-1])^2*f1 + 3*p0(x-t[k])^2*f2 else
      p0(x-t[i])^3 - p0(x-t[k-1])^3*f1 + p0(x-t[k])^3*f2
  if(scale)
    ret = ret/(t[k]-t[1])^2
  return(ret)
}
