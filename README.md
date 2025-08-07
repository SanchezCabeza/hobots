# hobots

\#<!-- badges: start --> \# \#<!-- badges: end -->

The goal of hobots is to Process Environmental Data from Observatories

The `hobots` package creates time series from Hobo sensor data at the Mazatlan observatory, starting with temperature from a single site, with plans to expand to multiple stations and variables (e.g., oxygen, temperature) from up to 5 sensors across 11 stations.

## Installation

``` r
install.packages("devtools")
devtools::install_github("SanchezCabeza/hobots")
```

## Example

``` r
library(hobots)
#Parse filename
parse_filename("mzt.1.1m.ot.20200101.20200202.csv")
```

## Development Plan

As of 2025-08-06, the package focuses on temperature data from a single Mazatlan site. Future updates will include support for multiple stations and additional variables like oxygen and pH.

``` r
```

## Example

Pending

``` r
library(hobots)
## basic example code
```
