
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aermod

<!-- badges: start -->

<!-- badges: end -->

This is a post-processor for AMS/EPA AERMOD. It works with unformatted
binary POSTFILEs. Its main application is to enable the use of Monte
Carlo methods for determining the ambient impacts of non-continuous
sources in SO<sub>2</sub> attainment demonstration modeling.

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
tabulate the design value for each AERMOD run. There are three basic
problems with such an approach: first, the samples will not be random;
second, the maximum number of simulations is constrained by the
computing overhead associated with re-running AERMOD for each
simulation; and third, the results will not be reproducible.

This package enables random, fast, and reproducible Monte Carlo
simulations to be run for regulatory applications.

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

### Converting POSTFILEs to Impact Matrices

AERMOD POSTFILEs are [FORTRAN 77 Unformattted
I/O](https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnc4/index.html)
binary files whose structure is described in the AERMOD user’s guide.
They are readily converted into R matrices.

``` r
#Convert some POST files  into impact matrices
# If the POST file was generated on a machine with different endianness than your own,
# supply endian="big" or endian="little" (endianness of the generating machine)
UIM_STK1 <- scan_postfile('STK1.bin')
UIM_STK2 <- scan_postfile('STK2.bin')
BACKGROUND <- scan_postfile('BACKGROUND.bin')

#Verify number of hours x receptors in the converted POST file
dim(UIM_STK1)
```

To avoid cluttering the global environment and to facilitate automatiion
and memory management, it may be desirable to collect impact matrices in
an environment:

``` r
all_impact_mats <- rlang::new_environment()
c('STK1.bin','STK2.bin','BACKGROUND.bin') %>% purrr::walk( function(.x) all_impact_mats[[ .x ]] <- scan_postfile(.x) )
```

The package can be used to calculate SO<sub>2</sub> design values for
deterministic runs, without running a Monte Carlo simulation. Note that
the impact matrices can be scaled and added, so long as they are
conformable.

``` r
get_dv(700 * UIM_STK1 + 100 * UIM_STK2 + BACKGROUND)
```

The functions `get_dv` and `fast_so2_dv` both calculate SO<sub>2</sub>
design values. The former can produce more useful diagnostic output,
while the latter is faster.

### Making a Simulation Plan

A simulation plan is a list of the randomly chosen hours (and emission
rates, if desired) for each simulation, as well as the value of
.Random.Seed leads to the generated hours. The code below generates a
simulation plan for 1000 simulations for two non-continuous sources: one
will emit for four hours, twenty times per calendar year; while the
other will emit for one calendar day, four times per calendar year.

``` r
sim.1 <- make_simplan(N_sim=1000,nblock=20,blocklen=4,units="hours")
sim.2 <- make_simplan(N_sim=1000,nblock=4,blocklen=1,units="days")
```

### Running the Monte Carlo Trials

The `blend` function creates an impact matrix consisting of specified
rows from the source matrix inserted into the target matrix (which is a
zero matrix by default). This function, as well as the design value
calculation function, is called repeatedly to generate the result set:

``` r
future::plan('multisession')
tic()
results <- furrr::map_df( 1L:1000L , 
    function (.x) fast_so2_dv( blend(UIM_STK1, sim.1[[ .x ]]$hrs) + 
            blend(UIM_STK2, sim.2[[ .x ]]$hrs ) + 
            BACKGROUND ), 
    .options=furrr::future_options(seed=TRUE) )
toc()
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
