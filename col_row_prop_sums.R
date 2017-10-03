#create function that returns summaries of
#a table
col_row_prop_sums = function(x, which="column", type="percent", na.rm=FALSE){
  which = match.arg(which,c("row","column"))
  if(length(which) > 1){
    which = which[1]
    warning("Only first value of 'which' used (", which, ").")
  }
  type = match.arg(type,c("proportion","percent"))
  if(length(type) > 1){
    type = type[1]
    warning("Only first value of 'type' used (", type, ").")
  }
  if(!is.matrix(x) || !is.numeric(x))
    stop("'x' must be a numeric matrix.")
  if(!is.logical(na.rm) || !length(na.rm))
    stop("'is.na' must be logical.")
  if(length(na.rm) > 1){
    na.rm = na.rm[1]
    warning("Only first value of 'na.rm' used (", na.rm, ").")
  }
  if(all(is.na(x))){
    warning("Values in 'x' are all missing.")
    return(x)
  }
  if(any(is.na(x)) && ! na.rm)
    warning("Missing values in 'x' are leading to missing values in output")
  margin_summary = if(which == "column")
    apply(x,2,sum,na.rm=na.rm) else
      apply(x,1,sum,na.rm=na.rm)
  x_prop = if(which == "column")
    sweep(x,2,margin_summary,"/") else
      sweep(x,1,margin_summary,"/")
  if(type == "percent")
    x_prop = x_prop * 100
  x_prop
}
