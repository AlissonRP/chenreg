# Intro
The most well-known regression models in the literature are based on the assumption
of normality. However, this assumption is not always satisfied in practice. As a consequence, the present package aims to develop the Chen quantile regression model, so a reparametrization of the Chen distribution in terms of the quantile was developed and a regression structure was inserted for its
modeling. 

## Installation
```r
devtools::install_github("https://github.com/AlissonRP/chenreg")
```
* ## Functions
 * `chen_reg`: This function is used to fit the Chen regression. It allows specifying the covariates of interest, the desired link function, and other relevant parameters for model fitting. The output of this function is an object of class `chenreg`, which encapsulates the results of the fitting process.
 * `rchen`: Generates simulated data from the Chen distribution. This function is particularly useful for conducting experiments, tests, and simulations involving the Chen quantile regression model. It allows users to generate datasets with specific and controlled characteristics.
 * `auto_chen`: Performs automatic covariate selection based on user-defined metrics of interest. This function is useful when dealing with a large number of available variables and aiming to find the optimal combination that maximizes the chosen metric.
 * `best_chen`: Fits the model with the best combination of variables selected from the models fitted in `auto_chen`. This function is useful when a well-optimized combination of variables is already known and the goal is to fit the model using only those selected covariates.
