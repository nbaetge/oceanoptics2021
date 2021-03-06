bb3
================
Nicholas Baetge
7/24/2021

# Load Libraries

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.2

    ## ── Attaching packages ──────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.0.3     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## Warning: package 'dplyr' was built under R version 4.0.2

    ## ── Conflicts ─────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.0.2

``` r
library(ggpubr)
library(fs)
```

    ## Warning: package 'fs' was built under R version 4.0.2

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
# library(zoo)
library(patchwork)
```

    ## Warning: package 'patchwork' was built under R version 4.0.2

``` r
library(viridis)
```

    ## Warning: package 'viridis' was built under R version 4.0.2

    ## Loading required package: viridisLite

# Import Data

also import corresponding acs data so that we can correct for absorption
later

``` r
acs_data <- read_rds("~/GITHUB/oceanoptics2021/data/acs_lab4/processed_acs_lab4.rds") %>% 
  filter(near(470, wl, tol = 2) | near(532, wl, tol = 2.5) | near(660, wl, tol = 2) ) %>% 
  select(ap, b) %>% 
  mutate(lambda = c(470, 532, 660)) %>% 
  pivot_wider(names_from = lambda, values_from = c(ap, b))
```

``` r
#save filenames that are in the folder of interest
csv_files <- fs::dir_ls("~/GITHUB/oceanoptics2021/data/bb3_lab4/", regexp = "\\.csv$")
```

``` r
#store all data from all files one data frame (this is what the map_dfr function does). We'll add the filenames to the dataframe as well, in a column called "source. 

import <- csv_files %>% 
  map_dfr(read.csv, skip = 2, header = F, .id = "source") %>% 
  #shorten the filenames
  mutate(source = gsub("/Users/nicholasbaetge/GITHUB/oceanoptics2021/data/bb3_lab4/", "", source),
         source = gsub(".csv", "", source)) %>% 
  #add some distinguisihing metadata about the filenames 
  mutate(sample = ifelse(source ==  "ARD_BB3349_20210722_184915", "ard a", NA),
        sample = ifelse(source ==  "ARD_BB3349_20210722_185132", "ard b", sample),
        sample = ifelse(source == "DARK_BB3349_20210722_195705", "dark", sample)) %>%
  select(source, sample, everything())

#add the headers to the dataframe
headers <- c("source", "sample", colnames(read_csv(csv_files[1])))
```

    ## Parsed with column specification:
    ## cols(
    ##   time = col_character(),
    ##   beta470 = col_character(),
    ##   beta532 = col_character(),
    ##   beta660 = col_character()
    ## )

``` r
colnames(import) <-  headers
```

# Beta - dark

``` r
data <- import %>% 
  group_by(sample) %>% 
  mutate(med_beta470 = median(beta470),
         se_beta470 = sd(beta470)/sqrt(length(beta470)),
         med_beta532 = median(beta532), 
         se_beta532 = sd(beta532)/sqrt(length(beta532)),
         med_beta660 = median(beta660),
         se_beta660 = sd(beta660)/sqrt(length(beta660)))  %>% 
  select(source, sample, contains(c("med", "se"))) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(dark_beta470 = ifelse(sample == "dark", med_beta470, NA),
         dark_beta532 = ifelse(sample == "dark", med_beta532, NA),
         dark_beta660 = ifelse(sample == "dark", med_beta660, NA)) %>% 
  fill(contains("dark"), .direction = "updown") %>% 
  filter(!sample == "dark") %>% 
  mutate(beta470_conv = 8.407E-06,
         beta532_conv = 4.624E-06,
         beta660_conv = 4.090E-06, 
         Beta470 = (med_beta470 - dark_beta470) * beta470_conv,
         Beta532 = (med_beta532 - dark_beta532) * beta532_conv,
         Beta660 = (med_beta660 - dark_beta660) * beta660_conv) %>%
  select(source, sample, Beta470:Beta660)
```

# Beta - sw

``` r
Tc = 20.5
lambda = c(470, 532, 660)
theta = 124

rad = theta*pi/180
```

``` r
vsf_sw <- rho::vsf_water(lambda = lambda, S = 0, Tc = Tc, psi = rad) %>% 
  as_data_frame() %>% 
  rename(vsf_sw = value) %>% 
  mutate(lambda = lambda) %>% 
  pivot_wider(names_from = lambda, values_from = vsf_sw) %>% 
  rename(corr470 = "470",
         corr532 = "532",
         corr660 = "660")
```

    ## Warning: `as_data_frame()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

# Beta - absorption

``` r
L = 0.0391
E = 0.4

library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
#values from sullivan and twardowski 2009
angle = c(90, 100, 110, 120, 130, 140, 150, 160, 170)
chi = c(0.684, 0.858, 1.00, 1.097, 1.153, 1.167, 1.156, 1.131, 1.093)
sd = c(0.034, 0.032, 0.026, 0.032, 0.044, 0.049, 0.054, 0.054, 0.057)

chi_table <- tibble(angle, chi, sd) %>% 
  add_row(angle = 135) %>% 
  arrange(angle) %>% 
  mutate(chi = na.approx(chi))

chi = 1.16


data_corr <- data %>% 
  bind_cols(., vsf_sw) %>% 
  mutate(Beta470_corr = Beta470 - corr470,
         Beta532_corr = Beta532 - corr532,
         Beta660_corr = Beta660 - corr660) %>%
  bind_cols(., acs_data) %>% 
  mutate(Beta470_abs_corr = Beta470_corr * exp(L * (ap_470 + E * b_470)),
         Beta532_abs_corr = Beta532_corr * exp(L * (ap_532 + E * b_532)),
         Beta660_abs_corr = Beta660_corr * exp(L * (ap_660 + E * b_660)),
         bbp470 = 2 * pi * chi * Beta470_abs_corr,
         bbp532 = 2 * pi * chi * Beta532_abs_corr,
         bbp660 = 2 * pi * chi * Beta660_abs_corr
         ) %>%
  select(sample, source, contains("bbp"))
```
