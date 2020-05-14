#' Plot an nbss object
#'
#' @param object result of function nbss()
#'
#' @export
#'
#' @return A ggplot object
autoplot.nbss <- function(object) {
  ggplot(object) +
    # geom_smooth(aes(x=bin, y=norm_y), data=ss[ss$status == "ok",], se=F, method="lm") +
    geom_point(aes(x=bin, y=norm_y)) +
    scale_shape_manual("Status", values=c(19, 4)) +
    scale_x_log10() +
    scale_y_log10() +
    labs(y=paste0("Normalised ", attr(object, "type")))
}

# h <- nbss(o$vol, binwidth=0.1)
# h
# autoplot(h)
