n_divergent <- function(x) {
  stopifnot(is(x, "brmsfit"))
  out <- lapply(x$fit@sim$samples, function(y) 
    sum(attr(y, "sampler_params")[["divergent__"]]))
  sum(unlist(out))
}