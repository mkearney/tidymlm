

fit_lm <- function(m) {
  if (inherits(m, "aov")) {
    s <- summary.lm(m)
  } else {
    s <- summary(m)
  }
  ## f and its p value
  f <- s$fstatistic[1]
  fp <- do.call("pf", as.list(c(unname(s$fstatistic), lower.tail = FALSE)))
  ## root mean square error
  rmse <- rmse(m)
  ## deviance
  #ll <- -2 * logLik(m)
  #lln <- as.integer(attr(ll, "df")
  # AIC/BIC
  aic <- AIC(m)
  bic <- BIC(m)
  ## stat name and estimate
  fit_statistic <- c("F", "R^2", "Adj R^2", "RMSE", "AIC", "BIC")
  estimate <- c(f, s$r.squared, s$adj.r.squared, rmse, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[c(1)], fit_statistic)] <- c(as.integer(s$fstatistic[2]))
  n <- nobs(m)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(c("F"), fit_statistic)] <- fp
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_stat = fit_statistic, n, df,
    estimate, p.value, stars)
}

fit_glm <- function(m) {
  s <- summary(m)
  devn <- s$df.residual
  devp <- pchisq(s$deviance, devn, lower.tail = FALSE)
  nulln <- s$df.null
  nullp <- pchisq(s$null.deviance, nulln, lower.tail = FALSE)
  chisq <- s$null.deviance - s$deviance
  chisqn <- nulln - devn
  chisqp <- pchisq(chisq, chisqn, lower.tail = FALSE)
  aic <- AIC(m)
  bic <- BIC(m)
  rmse <- rmse(m)
  r2nag <- nagelkerke(m)
  #r2cox <- coxsnell(m)
  r2mcf <- mcfadden(m)
  ##mcfadden.adj(m)
  ## names of fit statistics
  fit_statistic <- c("χ2","Δχ2", "Nagelkerke R^2",
    "McFadden R^2", "RMSE", "AIC", "BIC")
  ## estimates
  estimate <- c(s$deviance, chisq, r2nag, r2mcf, rmse, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[1:2], fit_statistic)] <- c(devn, chisqn)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(fit_statistic[1:2], fit_statistic)] <- c(devp, chisqp)
  ## number of obs
  n <- nobs(m)
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_stat = fit_statistic, n, df,
    estimate, p.value, stars)
}

