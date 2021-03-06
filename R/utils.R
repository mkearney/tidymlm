#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Convert object to tibble
#'
#' @inheritParams tibble::as_tibble
#' @param validate Logical, indicating whether to validate data frame.
#'   Defaults to \code{FALSE} because validation, while probably offering more
#'   safety and consistency, also provides a noticeable slow down in
#'   performance.
#' @return A tibble, class \code{c("tbl_df", "tbl", "data.frame")}
#' @export
as_tbl <- function(x, ..., validate = FALSE) {
  tibble::as_tibble(x, ..., validate = FALSE)
}

#' Create a tibble data frame
#'
#' @inheritParams tibble::tibble
#' @return A tibble, class \code{c("tbl_df", "tbl", "data.frame")}
#' @export
tbl_frame <- function(...) {
  xs <- rlang::quos(..., .named = TRUE)
  if (length(xs) == 1L) {
    x <- eval_tidy(xs[[1]])
    if (is.data.frame(x)) {
      return(as_tbl(x))
    }
  }
  as_tbl(tibble:::lst_quos(xs, expand = TRUE))
}

is_robust <- function(expr) {
  f <- rlang::expr_text(expr[[1]])
  ("estimator" %in% names(expr) && expr$estimator == "mlr") ||
    grepl("^robust\\:\\:glmRob$|^glmRob$|^MASS\\:\\:rlm$|^rlm$|^robustlmm\\:\\:rlmer$|^rlmer$", f)
}

is_ttest <- function(x) {
  grepl("^t|^htest|^ttest$", x, ignore.case = TRUE)
}

is_ols <- function(x) {
  grepl("^ols$|ordinary\\s?least\\s?squares", x, ignore.case = TRUE)
}

is_anova <- function(x) {
  grepl("anova|analysis of variance", x, ignore.case = TRUE)
}

is_log <- function(x) {
  grepl("^log$|^logistic|^binomial$", x, ignore.case = TRUE)
}

is_qlog <- function(x) {
  grepl("^quasi.?binom", x, ignore.case = TRUE)
}

is_pois <- function(x) {
  grepl("^pois$|poisson", x, ignore.case = TRUE)
}

is_qpois <- function(x) {
  grepl("^quasi.?pois", x, ignore.case = TRUE)
}

is_negbin <- function(x) {
  grepl("^negbin$|^negbinom$|negative\\s?binomial", x, ignore.case = TRUE)
}

is_sem <- function(x) {
  grepl("^sem$|structural equation|latent", x, ignore.case = TRUE)
}

is_mlm <- function(x) {
  grepl("^mlm$|multi.?level", x, ignore.case = TRUE)
}

std_model_type <- function(type) {
  type <- "mlm"
  type
}
