
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
#Convert some POST files (not provided) into R matrices
UIM_STK1 <- scan_postfile('STK1.bin')
UIM_STK2 <- scan_postfile('STK2.bin')
BACKGROUND <- scan_postfile('BACKGROUND.bin')

#Verify number of hours x receptors in the converted POST file
dim(UIM_STK1)

#Get SO2 Design value associated with a static scenario (scaled as desired)
get_dv(700 * UIM_STK1 + 100 * UIM_STK2 + BACKGROUND)

#A simulation plan consists of a set of randomly chosen hours (and emission rates, if desired), as
#well as the value of .Random.Seed that leads to their generation. To generate random hours
# only, use hrs_rand to get a tibble of hours and emission scalars
hrs_rand(nblock=20,blocklen=4,units="hours")

#Create one or more simulation plans 
simplan <- make_simplan(N_sim=100,nblock=21,blocklen=1,units="days")
# 100 runs, each consisting of (21) 1-day blocks (coinciding with calendar days)
simplan2 <- make_simplan(N_sim=100,nblock=1,blocklen=168,units="hours")
# 100 runs, each consisting of (1) 168-hour block
simplan3 <- make_simplan(N_sim=100,nblock=5,blocklen=1,units="days",emis_scale=c(750,750,200,200,200))
# 100 runs, each consisting of (5) 1-day blocks having binned emission rates of 750 lb/hr (2 days) and 200 lb/hr (3 days)

one_sim <- function(i) get_dv( blend( U_STK1 , simplan1[[i]]$hrs ) )
#Define a function to run a single simulation

purrr::map_dbl( 1:100 , ~ one_sim(.) )
# run it 100 times
```

## Installation

``` r
# install.packages("devtools")
devtools::install_github("jlovegren0/aermod")
```

## Example

``` r
library(aermod)
```