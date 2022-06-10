# Intro
The most well-known regression models in the literature are based on the assumption
of normality. However, this assumption is not always satisfied in practice. As a consequence, the present package aims to develop the Chen quantile regression model, so a reparametrization of the Chen distribution in terms of the quantile was developed and a regression structure was inserted for its
modeling. 

## Installation
```r
devtools::install_github("https://github.com/AlissonRP/chenReg")
```
* ## Functions
  * `chen_reg.fit()`: function to fit the  Quantile Regression
  * `chen_summary()`: Create a summary of the model in the format of `data.frame`
  * `chen_envlp`: Create a graph called a simulated envelope to see if the model fits the chen distribution well
  * `chen_rand`: Random numbers generation from the Chen distribution
