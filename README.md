
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aermod

<!-- badges: start -->

<!-- badges: end -->

Work with AERMOD unformatted binary POST files. Convert to/from R matrix
format, and calculate design values for the 2010 SO2 NAAQS. A typical
application is to generate a POST file for a unit run for each of
several sources, and use R matrix operations to scale, add, and
substitute rows.

Generate Monte Carlo runs where hours of operation are randomly turned
on/off for a source.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("jlovegren0/aermod")
```

## Example

``` r
library(aermod)
#UIM_STK1 <- scan_postfile('STK1.bin')
#UIM_STK2 <- scan_postfile('STK2.bin')
#BACKGROUND <- scan_postfile('BACKGROUND.bin')
#purrr::map_dbl( seq(100,500,100) , ~ 500*UIM_STK + . * UIM_STK2 + BACKGROUND )
```
