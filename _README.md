[![Travis-CI Build Status](https://travis-ci.org/jjongbloets/slideR.svg?branch=master)](https://travis-ci.org/jjongbloets/slideR)

SlideR
======

SlideR: An R package for Sliding Windows using dplyr, purrr and tidyr. The package creates a nested data.frame with the columns; key, w\_size and data.

Installation
------------

The package is in development and can be downloaded with `devtools`:

``` r
# install.packages("devtools")
devtools::install_git("https://www.gitlab.com/mmp-uva/slider.git")
```

### Requirements

The package requires the following packages:

-   `dplyr` (`install.packages("tidyverse")`)
-   `purrr` (`install.packages("purrr")`)
-   `tidyr` (`install.packages("tidtyr")`)

You can also install all these packages (and more) with one package:

-   `tidyverse` (`install.packages("tidyverse")`)

Usage
-----

To load the package:

``` r
library(slider)
```

### Partition data using a sliding window

``` r
x <- 1:100
slide_window(x, w_size=10)
```

When using data.frames, a key needs to specified. The key defines which column is used as key to the different windows.

``` r
df <- data.frame(x = 1:100, y=c("a", "b"))
slide_window(df, key="x", w_size=10)
```

### Partition data using multiple sliding windows

It is often useful to apply multiple sliding windows to the same data, while varying the size of the window.

This can be achieved using `slide_windows`

``` r
x <- 1:100
slide_windows(x, w_sizes=c(5, 10, 15))
```

### Apply a function to each window

In most cases the sliding window is used to apply a function to segments of the data. The function `apply_slide_window` and `apply_slide_windows` provide this functionality:

``` r
x <- 1:100
apply_slide_window(x, mean, w_size=10)
# or
apply_slide_windows(x, mean, w_sizes=c(5, 10, 15))
```

For data.frames it is a bit more complicated, as one needs to specify the column name within the window.

``` r
df <- data.frame(x = 1:100, y=c("a", "b"))
apply_slide_windows(df, ~mean(.$x), key="x", w_sizes=10)
```
