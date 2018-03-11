#' Print p-values nicely
#'
#' @param p the p-value
#' @param n number of digits afte leading zeros.
#'
#' @export
nice_p <- function(p, n=2) {
  order <- floor(log10(p))
  first_two_digits <- round(p * 10^-(order-(n-1)))
  paste0("p value: 0.", paste0(rep("0", -(order+1)), collapse=""), first_two_digits)
}
