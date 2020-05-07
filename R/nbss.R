#' Normalised Biomass Size Spectrum
#'
#' @param x vector of biomasses, biovolumes, or lengths.
#' @param type whether to compute a biomass/biovolume or abundance spectrum.
#' @param base base of the logarithm for the computation of bins.
#' @param binwidth width of bins in log10 scale.
#'
#' @return a data.frame with columns
#'
#' @export
#'
#' @examples
#' # Biovolume spectrum
#' ss <- nbss(uvp$volume_mm3)
#' head(ss)
#' autoplot(ss) + labs(
#'   x=expression("Biovolume (mm"^3*")"),
#'   y="Normalised biovolume"
#' )
nbss <- function(x, type=c("biomass", "abundance"), base=10, binwidth=0.1) {

  # check arguments
  type <- match.arg(type)
  if (! base %in% c(2,10)) {
    stop("`base` must be either 2 for natural logarithm or 10 for base 10 logarithm")
  }
  if (base == 2) {
    l <- log
    il <- exp
  } else {
    l <- log10
    il <- function(x) {10^x}
  }

  # log transform input data
  x_log <- l(x)

  # bin with precision `binwidth`
  x_log_bin <- round(x_log/binwidth) * binwidth

  # compute biomass/abundance in each bin
  if (type == "biomass") {
    fun <- sum
  } else {
    fun <- length
  }
  ss <- aggregate(x, list(x_log_bin), FUN=fun)
  names(ss) <- c("bin_log", "y")

  # compute middle of bin in the original scale
  ss$bin <- il(ss$bin_log)

  # compute bin widths in the original scale
  ss$binwidth <- il(ss$bin_log + binwidth/2) - il(ss$bin_log - binwidth/2)

  # compute the normalised biomass/volume
  ss$norm_y <- ss$y / ss$binwidth

  # reorder columns
  ss <- ss[,c("bin_log", "bin", "binwidth", "y", "norm_y")]

  # prepare output
  attr(ss, "type") <- type
  attr(ss, "base") <- base
  class(ss) <- c("nbss", class(ss))

  return(ss)
}
