
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aermod

<!-- badges: start -->

<!-- badges: end -->

Work with AMS/EPA AERMOD unformatted binary POST files. Convert to/from
R matrix format, and calculate design values for the 2010 SO2 NAAQS. A
typical application is to generate a POST file for a unit run for each
of several sources, and use R matrix operations to scale, add, and
substitute rows.

Generate Monte Carlo runs where hours of operation are randomly turned
on/off for a source. This is done by generating random hours
corresponding to row indices of a unit impact matrix:

``` r
# Generate 72 hours per year of random operation for calendar years 2016--2020
# (the default year span).
hrs_rand(yrspan=2016:2020,nblock=12,blocklen=1)

# Generate one block per year of 20 consecutive operating hours, 
# subject to a restriction that blocks must be 365 days apart (which_filter=2).
hrs_rand(nblock=1,blocklen=10,which_filter=2)
```

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
#
```
