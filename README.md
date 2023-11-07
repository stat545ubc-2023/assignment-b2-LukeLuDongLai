
# groupmissing

<!-- badges: start -->
<!-- badges: end -->

The `groupmissing` package provides a simple and effective way to count
missing values in each column of a dataset, grouped by a specific
factor.

## Installation

You can install the development version of `groupmissing` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/stat545ubc-2023/groupmissing")
```

## Examples

Here are basic examples which show you how to solve a common problem
using ‘count_all_missing_by_group’ function in ‘groupmissing’ package:

``` r
library(groupmissing)
# Assuming 'data' is your data.frame and 'group' is the grouping variable: count_all_missing_by_group(data, group)
```

This example computes the number of missing values in the `airquality`
dataset grouped by the `cyl` column.

``` r
count_all_missing_by_group(airquality, Month)
```

    ## # A tibble: 5 × 6
    ##   Month Ozone Solar.R  Wind  Temp   Day
    ##   <int> <int>   <int> <int> <int> <int>
    ## 1     5     5       4     0     0     0
    ## 2     6    21       0     0     0     0
    ## 3     7     5       0     0     0     0
    ## 4     8     5       3     0     0     0
    ## 5     9     1       0     0     0     0

This example has the same output as the last example, but shows off an
alternative way of invoking the `count_all_missing_by_group()`: piping
the dataset into the function.

``` r
airquality |> count_all_missing_by_group(Month) 
```

    ## # A tibble: 5 × 6
    ##   Month Ozone Solar.R  Wind  Temp   Day
    ##   <int> <int>   <int> <int> <int> <int>
    ## 1     5     5       4     0     0     0
    ## 2     6    21       0     0     0     0
    ## 3     7     5       0     0     0     0
    ## 4     8     5       3     0     0     0
    ## 5     9     1       0     0     0     0

The optional `.groups` argument allows us to keep the output grouped by
the grouping column. See example below; notice how the output is a
grouped tibble, rather than the ungrouped tibble output of the earlier
examples.

``` r
count_all_missing_by_group(airquality, Month, .groups = "keep")
```

    ## # A tibble: 5 × 6
    ## # Groups:   Month [5]
    ##   Month Ozone Solar.R  Wind  Temp   Day
    ##   <int> <int>   <int> <int> <int> <int>
    ## 1     5     5       4     0     0     0
    ## 2     6    21       0     0     0     0
    ## 3     7     5       0     0     0     0
    ## 4     8     5       3     0     0     0
    ## 5     9     1       0     0     0     0
