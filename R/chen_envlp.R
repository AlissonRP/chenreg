#' chen_envlp
#'
#' Create a simulated envelope to see if the model fits the chen distribution well.
#'
#' @param z  a model created by `chen_reg.fit`
#'
#' @param  b  quantity of resamples
#'
#' @example
#' chenReg::chen_reg.fit(Y~.,link = 'log', tau = 0.5,data = chenReg::simu[,-1]) |>
#' chenReg::chen_envlp()
#'
#'@note by default the quantity of resamples is 100


#' @export
chen_envlp <- function(z, b = 100) {
  z$residual %>%
    as.data.frame() %>%
    ggplot2::ggplot(ggplot2::aes(sample = V1)) +
    qqplotr::geom_qq_band(alpha = 0.5, fill = "white", col = "black", B = b, bandType = "boot") +
    qqplotr::stat_qq_line(size = 0.5, linetype = "dashed") +
    qqplotr::stat_qq_point(size = 1.2) +
    ggplot2::scale_fill_discrete("Bandtype") +
    ggplot2::labs(x = "Quantis teóricos", y = "Quantis amostrais")
}



