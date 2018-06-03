
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidymlm <img src="man/figures/logo.png" width="160px" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

🎓 Tidy tools for
academics

## \*\*\* This package is in very early development. Feedback is encouraged\!\!\! \*\*\*

## Installation

<!-- You can install the released version of tidymlm from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidymlm")
```
-->

Install the development version from
[Github](https://github.com/mkearney/tidymlm) with:

``` r
## install devtools if not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install tidymlm from Github
devtools::install_github("mkearney/tidymlm")
```

Load the package (it, of course, plays nicely with tidyverse).

``` r
## load tidyverse
library(tidyverse)

## load tidymlm
library(tidymlm)
```

### Multilevel modeling (MLM)

Estimate multilevel (mixed effects) models.

``` r
lme4::sleepstudy %>%
  tidy_mlm(Reaction ~ Days + (Days | Subject)) %>%
  summary()
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: Reaction ~ Days + (Days | Subject)
#>    Data: .data
#> 
#> REML criterion at convergence: 1743.6
#> 
#> Scaled residuals: 
#>    Min     1Q Median     3Q    Max 
#> -3.954 -0.463  0.023  0.463  5.179 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev. Corr
#>  Subject  (Intercept) 612.1    24.74        
#>           Days         35.1     5.92    0.07
#>  Residual             654.9    25.59        
#> Number of obs: 180, groups:  Subject, 18
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)   251.41       6.82   36.84
#> Days           10.47       1.55    6.77
#> 
#> Correlation of Fixed Effects:
#>      (Intr)
#> Days -0.138
```
