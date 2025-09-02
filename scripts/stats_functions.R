# calculate unbiased standard deviation from small sample set
calculate_unbiased_sd <- function(sd, n) {
  cn <- if_else(
    n > 300, 1,
    sqrt(2/(n - 1)) * gamma(n/2) / gamma((n - 1) / 2)
  )
  return(sd/cn)
}

# calculate pooled standard deviation based on multiple sds from small number replicate analyses
calculate_pooled_sd <- function(sds, ns, bias_correct = TRUE) {
  # there's no sdev for sample size 1
  sds <- sds[ns > 1]
  ns <- ns[ns > 1]
  if (bias_correct) sds <- calculate_unbiased_sd(sds, ns)
  return(sqrt( sum((ns - 1) * sds^2, na.rm = TRUE) / sum(ns - 1)))
} 

# calculate standard error of the mean, typically from pooled sd
calculate_sem <- function(sd, n) {
  return(sd/sqrt(n))
}
