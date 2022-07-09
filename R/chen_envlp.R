#' chen_envlp
#'
#' Create a simulated envelope to see if the model fits the  Chen distribution well.
#'
#' @param z  a model created by `chen_reg`
#'
#' @param  b  quantity of resamples
#'
#' @examples
#' chenReg::chen_reg(formula = Y ~., link = 'log', tau = 0.5,
#' data = chenReg::simu[,-1]) |> chenReg::chen_envlp()
#'
#' @note by default the quantity of resamples is 100


#' @export
chen_envlp <- function(z, b = 100) {
  z$residual %>%
    as.data.frame() %>%
    ggplot2::ggplot(ggplot2::aes(sample = V1)) +
    qqplotr::geom_qq_band(alpha = 0.5, fill = "white", col = "black", B = b, bandType = "boot") +
    qqplotr::stat_qq_point(size = 1.2) +
    ggplot2::scale_fill_discrete("Bandtype") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
}


