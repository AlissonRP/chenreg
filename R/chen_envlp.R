#' Simulated envelop for chen distribution
#'
#' @param z 	a model created by chen_reg.fit
#'                  a symbolic description of the model to be fitted
#' @param  b   Quantity of resamples



#' @export
chen_envlp=function(z,b){
z$residual %>%
  as_tibble() %>%
  ggplot(aes(sample = V1)) +
  qqplotr::geom_qq_band(alpha = 0.5, fill="white", col="black",B=b,bandType = "boot") +
  qqplotr::stat_qq_line(size=0.5, linetype="dashed") +
  qqplotr::stat_qq_point(size=1.2) +
  scale_fill_discrete("Bandtype")+
  labs(x = "Quantis teóricos", y = "Quantis amostrais") }
