
#' chen_reg.fit
#'
#' function to fit the  Quantile Regression
#'
#' @param formula 	an object of class "formula" (or one that can be coerced to that class):
#'                  a symbolic description of the model to be fitted
#' @param  data   an optional data frame, list or environment (or object coercible by as.data.frame to a data frame)
#'                containing the variables in the model.
#' @param tau     a number that indicates the quantile that you want to fit the regression
#' @param link    a string that indicates the link function that you want to fit the regression
#'
#' @return não sei
#'
#' @export
chen_reg.fit <- function(formula, data, tau, link) {

  # ==================== Escolha da Funcao de ligacao==================#
  if (link == "log") {
    ginv_lig <- function(c) {
      exp(c)
    }
    g_lig <- function(c) {
      log(c)
    }
  }
  if (link == "sqrt") {
    ginv_lig <- function(c) {
      c^2
    }
    g_lig <- function(c) {
      sqrt(c)
    }
  }

  data <- data[, c(which(colnames(data) == formula[[2]]), which(colnames(data) != formula[[2]]))]
  y <- data[, 1] %>%
    unlist()
  n <- length(y)
  if (min(y) < 0) {
    stop("OUT OF RANGE!")
  }
  ## ===== Chute ======
  X <- model.matrix(formula, data)
  mqo <- lm.fit(as.matrix(X), unlist(log(data[, formula[[2]]]))) %>%
    .$coefficients
  lambdac <- 0.6


  #### lvero##### será removida futuramente (srf)
  lvero <- function(theta, data) {
    n <- nrow(data[, 1])
    lambda <- theta[1]

    beta <- theta[2:length(theta)]

    eta <- X %*% as.matrix(beta)
    md <- ginv_lig(eta)

    lv <- suppressWarnings(log(log(1 - tau) / (1 - exp(md^lambda))) + log(lambda) + (lambda - 1) * log(y) +
                             (log(1 - tau) / (1 - exp(md^lambda))) * (1 - exp(y^lambda)) + (y^lambda))
    return(sum(lv))
  }


  escore <- function(theta, data) {
    lambda <- theta[1]
    beta <- theta[2:length(theta)]

    eta <- as.vector(X %*% as.matrix(beta))
    md <- ginv_lig(eta)
    mB <- as.vector(-(lambda * md^(lambda - 1) * exp(md^lambda) * (exp(md^lambda) +
      log(1 - tau) * exp(y^lambda) - log(1 - tau) - 1)) / ((exp(md^lambda) - 1)^2))
    mL <- as.vector(((-log(1 - tau) * y^lambda * log(y) * exp(y^lambda) + (md^lambda) * log(md) * exp(md^lambda)) / (1 - exp(md^lambda)))
    + ((log(1 - tau) * (md^lambda) * log(md) * exp(md^lambda) * (1 - exp(y^lambda))) / ((1 - exp(md^lambda))^2)) +
      1 / lambda + y^lambda * log(y) + log(y))


    mT <- diag(exp(eta))

    Ulambda <- sum(mL)
    Ubeta <- t(X) %*% mT %*% mB

    rval <- c(Ulambda, Ubeta)
    return(rval)
  }

  lvero2 <- function(param) {
    lambda <- param[1]
    md <- param[2]
    lv2 <- suppressWarnings(log(log(1 - tau) / (1 - exp(md^lambda))) + log(lambda) + (lambda - 1) * log(y) +
      (log(1 - tau) / (1 - exp(md^lambda))) * (1 - exp(y^lambda)) + (y^lambda))
    sum(lv2)
  }

  par <- round(c(as.numeric(lambdac), as.numeric(mqo)), 2)


  tempo <- Sys.time()
  opt <- optim(par, lvero,
    data = data, gr = escore, method = "BFGS", hessian = T,
    control = list(fnscale = -1, maxit = 2000, reltol = 1e-10)
  )


  if (opt$conv != 0) {
    warning("FUNCTION DID NOT CONVERGE!")
  }


  z <- c()
  z$conv <- opt$conv
  coefficients <- (opt$par)[1:(1 + ncol(X))]
  names(coefficients) <- c("lambda", c(paste("beta", 1:ncol(as.matrix(X)), sep = "")))
  z$coefficients <- coefficients

  lambda <- coefficients[1]
  beta <- coefficients[2:length(coefficients)]

  z$lambda <- lambda

  etahat <- X %*% as.matrix(beta) # estimativa do g(b;x)
  muhat <- ginv_lig(etahat) # estimativa do q=g^-1(b;x)

  ## =====C?lculo do R2===================================================


  opt0 <- optim(c(lambda, muhat), lvero2, control = list(fnscale = -1, maxit = 1000), method = "BFGS") # emv sem reg


  R2_calc <- 1 - exp(-(2 / n) * (opt$value - opt0$value))


  z$fitted.values <- muhat
  z$etahat <- etahat
  z$serie <- y
  z$X <- X
  z$chen <- names(coef)
  z$tau <- tau
  z$link <- link

  ## =======================================================================================
  # res?duo quant?lico

  delta <- (log(1 - tau)) / (1 - (exp(muhat^lambda)))
  z$residual <- qnorm(pchen(y, b = lambda, lambda = delta))
  residc <- z$residual



  z$vcov <- -opt$hessian %>%
    solve()

  z$stderror <- z %>%
    .$vcov %>%
    diag() %>%
    sqrt()


  z$zstat <- abs(z$coeff / z$stderror)
  z$pvalues <- 2 * (1 - pnorm(z$zstat))

  z$loglik <- opt$value
  z$counts <- as.numeric(opt$counts[1])
  z$aic <- -2 * z$loglik + 2 * (1 + length(beta))
  z$bic <- -2 * z$loglik + log(n) * (1 + length(beta))
  z$r2 <- R2_calc


  model_presentation <- cbind(round(z$coef, 4), round(z$stderror, 4), round(z$zstat, 4), round(z$pvalues, 4))
  colnames(model_presentation) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

  z$presentation <- function(...) {
    print(model_presentation)
    cat(" \n")
    cat(paste0("Log-likelihood: ",round(z$loglik, 4)),"\n")
    cat(c("Number of iterations in BFGS optim:", z$counts),"\n")
    cat(c("AIC:", round(z$aic, 4), " BIC:", round(z$bic, 4)), "\n")
    cat("Residuals:\n")
    print(summary(as.vector(residc)))
    cat(c("R-squared:",round(z$r2, 4)))
  }

z$call=match.call()


  print.lm <- function( digits = max(3L, getOption("digits") - 3L))
  {
    cat("\nCall:\n",
        paste(deparse(z$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
      cat("Coefficients:\n")
      print.default(format(coefficients, digits = digits),
                    print.gap = 2L, quote = FALSE)

  }

z$call=print.lm()






}
