coef_default <- function(x) {
  ## broom the coef table, rename, and add stars column
  d <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(d)[2:4] <- c("est", "s.e.", "est.se")
  d <- add_stars(d)
  ## estimate/add standardized solution estimates and return
  add_std_est(d)
}


