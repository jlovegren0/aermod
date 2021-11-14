
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aermod

<!-- badges: start -->

<!-- badges: end -->

This is a post-processor for AMS/EPA AERMOD. It works with unformatted
binary POSTFILEs. Its main application is to enable the use of Monte
Carlo methods for determining the ambient impacts of non-continuous
sources in SO2 attainment demonstration modeling.

## Monte Carlo Methods in Regulatory Dispersion Modeling

Built-in AERMOD algorithms permit the modeling of non-continuous sources
with *fixed operating schedules* via the EMISFACT or HOUREMIS card: for
example, a source that only operates during daytime hours or does not
operate during the winter months. Sources that are subject to a limit on
the total number of hours or days of operation, but do not operate to a
fixed schedule, however, cannot be accommodated with AERMOD’s built-in
algorithms.

To illustrate the problem, consider the example of a hypothetical
SO<sub>2</sub> source that is authorized to operate during no more than
than four calendar days in a given year. Considering a five-year
meteorological database spanning 2016–2020, the number of unique ways of
assigning the hours of operation to hours of the meteorological database
is   
![{366\\choose 4}^2{365\\choose 4}^3
= 208226519296227450624822934282939747178381312](https://latex.codecogs.com/png.latex?%7B366%5Cchoose%204%7D%5E2%7B365%5Cchoose%204%7D%5E3%20%3D%20208226519296227450624822934282939747178381312
"{366\\choose 4}^2{365\\choose 4}^3 = 208226519296227450624822934282939747178381312")  
each of which is assumed to be equally probable. While it is not
possible to model all scenarios, it is problematic to completely
disregard the 4 day/yr restriction in the modeling (i.e., by modeling
the source as though it operated continuously).

If it **were** possible to evaluate all of the possible operating
schedules, the aim in doing so so would be to determine the distribution
function of the resulting design values and compute the desired
statistics on it. Of course, it is possible to do this using standard
statistical methods on a representative sample of the possible operating
schedules. Monte Carlo methods are used to generate a sufficient number
of random outcomes such that the desired statistics can be computed.

It can fairly be asked whether a specialized post-processor is necessary
to conduct a Monte Carlo analysis. After all, the same type of analysis
can be performed using a “brute force” approach: construct a number of
operating schedules thought to be representative; run AERMOD for each of
the selected schedules, using the EMISFACT or HOUREMIS cards; and then
tabulate the design value for each AERMOD run. There are two basic
problems with such an approach: first, the samples will not be random;
and second, the maximum number of simulations is constrained by the
computing overhead associated with re-running AERMOD for each
simulation.

## Implementation Details

Running a Monte Carlo simulation consists of four steps.

1.  Binary POSTFILEs files are converted into impact matrices.
2.  A simulation plan is generated, consisting of randomly selected
    hours of operation for each simulation trial
3.  Monte Carlo trials are run, each trial consisting of two steps:
      - Impact matrices corresponding to non-continuous sources are
        altered so that concentration corresponding to a non-operating
        hours is zero.
      - All relevant impact matrices are summed, and a design value for
        the total impact matrix is calculated.
4.  The desired statistics are calculated on the result set.

<!-- end list -->

``` r
#Convert some POST files (not provided) into impact matrices
# If the POST file was generated on a machine with different endianness than your own,
# supply endian="big" or endian="little" (endianness of the generating machine)
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

#The blend function creates an impact matrix consisting of specified rows from the source matrix
#inserted into the target matrix (which is a zero matrix by default)
blend(U_STK1, hrs_rand(nblock=20,blocklen=4,units="hours"), UIM_STK2)

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

#For larger simulation sets, use parallel computation
future::plan('multisession')
simplan3 <- make_simplan(N_sim=10000,nblock=5,blocklen=1,units="days",emis_scale=c(750,750,200,200,200))
one_sim <- function(i) get_dv( blend( U_STK1 , simplan3[[i]]$hrs ) )
furrr::future_map_dbl( 1:10000 , ~ one_sim(.),.options=furrr::future_options(seed=TRUE) )
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
