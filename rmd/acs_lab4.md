acs\_lab4
================
Nicholas Baetge
7/22/2021

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
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(patchwork)
```

    ## Warning: package 'patchwork' was built under R version 4.0.2

# Import Data

``` r
#save filenames that are in the folder of interest
csv_files <- fs::dir_ls("~/GITHUB/oceanoptics2021/data/acs_lab4/good/", regexp = "\\.csv$")
```

``` r
#the first two rows of the acs data output contain header and unit data. we'll store those separately...units first and we'll circle back to headers
units <- colnames(read_csv(csv_files[1], skip = 1))
```

    ## Warning: Duplicated column names deduplicated: 'deg_C' => 'deg_C_1' [6]

    ## Parsed with column specification:
    ## cols(
    ##   `yyyy/mm/dd HH:MM:SS.fff` = col_character(),
    ##   ms = col_double(),
    ##   `1/m   lambda=400.5 404.8 408.9 412.6 416.9 421.7 426.5 431.1 435.7 440.0 444.9 450.0 454.8 459.4 464.0 468.5 473.8 479.0 484.1 489.0 493.5 498.6 503.5 508.5 513.7 519.4 524.2 529.2 533.9 538.9 544.0 549.0 554.1 559.3 564.5 569.3 573.6 578.1 582.5 586.9 591.2 595.5 600.3 605.0 609.8 614.7 619.7 624.5 629.2 633.7 638.5 643.1 647.9 652.6 657.0 661.6 666.4 670.8 675.4 679.8 683.8 687.9 692.0 695.5 699.6 703.4 707.1 710.7 714.4 717.7 721.1 724.3 727.5 730.5 733.4 736.4 739.2 742.1 744.8 747.6 749.8` = col_character(),
    ##   `1/m   lambda=400.0 404.6 408.7 412.5 416.9 421.4 426.5 430.9 435.5 439.8 444.8 449.9 455.0 459.4 463.8 468.7 474.0 479.2 484.1 489.2 493.9 498.9 503.8 508.7 514.0 519.7 524.8 529.9 534.6 539.6 544.4 549.7 554.6 560.1 565.2 569.8 574.2 579.0 583.4 587.8 592.7 597.4 602.1 607.0 612.0 616.9 621.9 626.5 631.2 635.8 640.8 645.3 650.2 654.9 659.3 663.9 668.8 673.2 677.6 681.9 686.1 690.0 693.9 697.8 701.8 705.5 709.1 712.8 716.6 719.8 723.2 726.5 729.4 732.3 735.3 738.3 741.3 744.2 746.7 749.4 751.9` = col_character(),
    ##   deg_C = col_double(),
    ##   deg_C_1 = col_double(),
    ##   bool = col_logical()
    ## )

``` r
#extract the wavelengths from the "a" column and store as a vector (these are also the same for c)
wavelengths <- units[4] %>% strsplit(., split = " ") %>% .[[1]] %>%  str_remove(., "1/m lambda=") 
   
#store all data from all files one data frame (this is what the map_dfr function does). We'll add the filenames to the dataframe as well, in a column called "source. 
data <- csv_files %>% 
  map_dfr(read.csv, skip = 2, header = F, .id = "source") %>% 
  #shorten the filenames
  mutate(source = gsub("/Users/nicholasbaetge/GITHUB/oceanoptics2021/data/acs_lab4/good/", "", source),
         source = gsub(".csv", "", source)) %>% 
  #add some distinguisihing metadata about the filenames 
  mutate(sample = ifelse(source ==  "ARD_a_2_ACS94_20210722_195827", "ard a", NA),
        sample = ifelse(source ==  "ARD_c_2_ACS94_20210722_202755", "ard c", sample),
     
        sample = ifelse(source ==  "DIWARD_cal_a_2_ACS94_20210722_194452", "cal a", sample),
        sample = ifelse(source ==  "DIW_ACS94_cal_c_2_20210722_202006", "cal c", sample)) %>%
  select(source, sample, everything())

#add the headers to the dataframe
headers <- c("source", "sample", colnames(read_csv(csv_files[1])))
```

    ## Parsed with column specification:
    ## cols(
    ##   time = col_character(),
    ##   acs_timestamp = col_character(),
    ##   c = col_character(),
    ##   a = col_character(),
    ##   T_int = col_character(),
    ##   T_ext = col_character(),
    ##   flag_outside_calibration_range = col_character()
    ## )

``` r
colnames(data) <-  headers
```

# c

follows process as for a

``` r
c <- data %>% 
  select(c) %>% 
  mutate(c = str_squish(c)) %>% 
  #remove the brackets
  mutate(c = str_replace_all(c, "\\[|\\]", "")) %>%
  mutate(c = str_squish(c)) %>% 
  separate(c, sep = " ", into = wavelengths) %>% 
  mutate_all(as.numeric)
```

``` r
c_data <- bind_cols(data, c) %>% 
  select(-c, -a) %>% 
  mutate(time = ymd_hms(time)) %>% 
  group_by(source) %>% 
   mutate(interv = interval(first(time), time),
         s = as.numeric(interv),
         min = s/60) %>% 
  select(source:time, interv:min, everything()) %>% 
  ungroup() 
```

## medians and se

``` r
c_med <- c_data %>% 
  select(source:sample, c(11:91)) %>% 
  group_by(source) %>% 
  mutate_at(c(3:83), list(med = median)) %>% 
  ungroup() %>% 
  select(source:sample, contains("med")) %>% 
  distinct() %>% 
  rename_at(.vars = vars(ends_with("_med")),
            .funs = funs(sub("_med", "", .)))
```

    ## Warning: `funs()` is deprecated as of dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
c_se <- c_data %>% 
  select(source:sample, c(11:91)) %>% 
  group_by(source) %>% 
  mutate_at(c(3:83), list(se = function(x) sd(x) / sqrt(length(x)))) %>% 
  ungroup() %>% 
  select(source:sample, contains("se")) %>% 
  distinct() %>% 
  rename_at(.vars = vars(ends_with("_se")),
            .funs = funs(sub("_se", "", .)))

c_pivot <- c_med %>% 
  pivot_longer(c(3:83), names_to = "wl", values_to = "c" ) %>%
  mutate_at(vars(c), as.numeric) %>% 
  bind_cols(., 
            c_se %>% 
  pivot_longer(c(3:83), names_to = "wl", values_to = "se" ) %>%
  mutate_at(vars(se), as.numeric) %>% select(-source, -sample, -wl)) %>% 
  mutate_at(vars(wl), as.numeric)
```

## derive cp

``` r
cp_data <- c_pivot %>% 
  filter(sample %in% c("cal c", "ard c")) %>% 
  mutate(total_c = ifelse(sample == "ard c", c, NA),
         cdom_c = ifelse(sample == "cal c", c, NA),
         se_total_c = ifelse(sample == "ard c", se, NA)) %>% 
  group_by(wl) %>% 
  fill(c(total_c, cdom_c, se_total_c), .direction = "updown") %>% 
  ungroup() %>% 
  select(source, sample, wl, total_c, cdom_c, se_total_c) %>% 
  mutate(cp = total_c - cdom_c)
```

# a

``` r
#extract the a-tube data and separate out the data
a <- data %>% 
  select(a) %>% 
  #there are sometimes one, two, or three spaces between the data. str_squish helps us deal with that
  mutate(a = str_squish(a)) %>% 
  #remove the brackets
  mutate(a = str_replace_all(a, "\\[|\\]", "")) %>%
  mutate(a = str_squish(a)) %>% 
  separate(a, sep = " ", into = wavelengths) %>% 
  mutate_all(as.numeric)
```

``` r
#combine a data with metadata and change data classes
a_data <- bind_cols(data, a) %>% 
  select(-c, -a) %>% 
  mutate(time = ymd_hms(time)) %>% 
  group_by(source) %>% 
   mutate(interv = interval(first(time), time),
         s = as.numeric(interv),
         min = s/60) %>% 
  select(source:time, interv:min, everything()) %>% 
  ungroup() 
```

## medians and se

``` r
#calculate medians for each wavelength
a_med <- a_data %>% 
  select(source:sample, c(11:91)) %>% 
  group_by(source) %>% 
  mutate_at(c(3:83), list(med = median)) %>% 
  ungroup() %>% 
  select(source:sample, contains("med")) %>% 
  distinct() %>% 
  rename_at(.vars = vars(ends_with("_med")),
            .funs = funs(sub("_med", "", .)))
```

``` r
#calculate se
a_se <- a_data %>% 
  select(source:sample, c(11:91)) %>% 
  group_by(source) %>% 
  mutate_at(c(3:83), list(se = function(x) sd(x) / sqrt(length(x)))) %>% 
  ungroup() %>% 
  select(source:sample, contains("se")) %>% 
  distinct() %>% 
  rename_at(.vars = vars(ends_with("_se")),
            .funs = funs(sub("_se", "", .)))

#combine median and se data and melt for plotting
a_pivot <- a_med %>% 
  pivot_longer(c(3:83), names_to = "wl", values_to = "a" ) %>%
  mutate_at(vars(a), as.numeric) %>% 
  bind_cols(., 
            a_se %>% 
  pivot_longer(c(3:83), names_to = "wl", values_to = "se" ) %>%
  mutate_at(vars(se), as.numeric) %>% select(-source, -sample, -wl)) %>% 
  mutate_at(vars(wl), as.numeric)
```

## derive ap

``` r
ap_data <- a_pivot %>% 
  filter(sample %in% c("cal a",  "ard a")) %>% 
  mutate(total_a = ifelse(sample == "ard a", a, NA),
         cdom_a = ifelse(sample == "cal a", a, NA),
         se_total_a = ifelse(sample == "cal a", se, NA)) %>% 
  group_by(wl) %>% 
  fill(c(total_a, cdom_a, se_total_a), .direction = "updown") %>% 
  ungroup() %>% 
  select(source, sample, wl, total_a, cdom_a, se_total_a) %>% 
  mutate(ap = total_a - cdom_a) 
```

## plot

``` r
ap_data %>%
  rename(se = se_total_a) %>% 
  select(wl, total_a, ap, se) %>% 
  ggplot(aes(x = wl, y = ap)) +
  geom_errorbar(aes(ymin = ap - se, ymax = ap + se), size = 2, width = 1) +
  geom_line() +
  geom_line(aes(y = total_a), color = "blue") +
  geom_errorbar(aes(ymin = total_a - se, ymax = total_a + se), size = 2, width = 0.5, color = "blue") +
  labs(x = "wavelength") +
  theme_classic2(20)
```

![](acs_lab4_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# combine ap and cp data

``` r
combined <- bind_cols(ap_data, cp_data %>% select(-c(source, sample, wl))) %>% 
  mutate(b = cp - ap) %>% 
  filter(sample == "ard a")
```

## correct for temperature and scattering

### sullivan et al 2006 values

``` r
#sullivan et al 2006 values

psi_wl <-  c(400,402,404,406,408,410,412,414,416,418,420,422,424,426,428,430,432,434,436,438,440,442,444,446,448,450,452,454,456,458,460,462,464,466,468,470,472,474,476,478,480,482,484,486,488,490,492,494,496,498,500,502,504,506,508,510,512,514,516,518,520,522,524,526,528,530,532,534,536,538,540,542,544,546,548,550,552,554,556,558,560,562,564,566,568,570,572,574,576,578,580,582,584,586,588,590,592,594,596,598,600,602,604,606,608,610,612,614,616,618,620,622,624,626,628,630,632,634,636,638,640,642,644,646,648,650,652,654,656,658,660,662,664,666,668,670,672,674,676,678,680,682,684,686,688,690,692,694,696,698,700,702,704,706,708,710,712,714,716,718,720,722,724,726,728,730,732,734,736,738,740,742,744,746,748,750)

psiT <-  c(0.0001,0.0001,0.0001,0.0001,0,0,0,0.0001,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0001,0.0001,0.0001,0.0002,0.0003,0.0003,0.0004,0.0005,0.0006,0.0006,0.0007,0.0008,0.0009,0.001,0.001,0.001,0.001,0.001,0.0009,0.0009,0.0008,0.0007,0.0006,0.0006,0.0005,0.0004,0.0003,0.0003,0.0002,0.0001,0.0001,0,0,0,0,0,0,0,0,0,0.0001,0.0001,0.0001,0.0002,0.0002,0.0002,0.0001,0.0001,0.0001,0,0,-0.0001,-0.0001,-0.0001,-0.0001,-0.0001,-0.0001,0,0,0.0001,0.0002,0.0003,0.0005,0.0007,0.0009,0.0013,0.0017,0.0021,0.0026,0.0032,0.0038,0.0045,0.0054,0.0063,0.0073,0.0083,0.0094,0.0104,0.0113,0.0121,0.0128,0.0133,0.0136,0.0136,0.0133,0.0129,0.0124,0.0116,0.0107)

wl <- as.numeric(wavelengths) %>% 
  data_frame(.) %>% 
  rename(psi_wl = ".") %>% 
  mutate(psiT = NA) 
```

    ## Warning: `data_frame()` is deprecated as of tibble 1.1.0.
    ## Please use `tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
temp <- data_frame(psi_wl, psiT) %>% 
  bind_rows(., wl) %>% 
  arrange(psi_wl) %>% 
  distinct(., psi_wl, .keep_all = T) %>% 
  zoo(., order.by = .$psi_wl) 
  
psiT_interp <-  as.numeric(na.approx(temp$psiT, na.rm = F)) 
psi_wl_interp  <- temp$psi_wl
interpolated_temp <- data.frame(psi_wl_interp, psiT_interp) %>% 
  rownames_to_column() %>% 
  rename(psi_wl = psi_wl_interp) %>% 
  select(-rowname) %>% left_join(wl %>% select(psi_wl), .) %>% 
  drop_na()
```

    ## Joining, by = "psi_wl"

``` r
to_correct <- combined %>%
  rename(psi_wl = wl) %>% 
  left_join(., interpolated_temp) 
```

    ## Joining, by = "psi_wl"

``` r
# Find Near Infrared & references
iNIR <- to_correct %>%  filter(between(psi_wl, 710, 750))  # spectral srange for optimization (710 to 750nm)

#Find nearest wavelength to greater than 715 nm to use as reference for correction
iref <- iNIR %>% filter(abs(psi_wl - 715) == min(abs(psi_wl - 715)))
```

\#\#\#\#cost function to find deltaT

``` r
deltaT = 0

cost_function <- function(iNIR, iref, deltaT) { sum(abs(iNIR$ap - iNIR$psiT_interp * deltaT - ((iref$ap - iref$psiT_interp * deltaT) / iref$b * iNIR$b))) }

optimize <- optim(fn = cost_function, iNIR = iNIR, iref = iref, par = deltaT, lower = 0, upper = 20000, method = "Brent") 

deltaT <- optimize$value
```

``` r
corrected <- to_correct %>% 
  mutate(deltaT = deltaT,
         iref_ap = iref$ap,
         iref_psiT = iref$psiT_interp,
         iref_b = iref$b,
         
         ap_corr = ap - psiT_interp * deltaT - (iref_ap - iref_psiT * deltaT) / iref_b * b, 
         cp_corr = cp - psiT_interp * deltaT)
```

### plots

``` r
a_plot <- corrected %>%
  ggplot(aes(x = psi_wl, y = ap_corr)) +
  # geom_errorbar(aes(ymin = ap - se, ymax = ap + se), size = 2, width = 1) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = ap_corr - se_total_a, ymax = ap_corr - se_total_a), size = 6, width = 0.5) +
  scale_y_continuous(expand = c(0,0)) +
  ylim(c(0, 4)) +
  labs(x = "Wavelength", y = expression(paste("a, m"^-1))) +
  theme_classic2(20)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
a_plot2 <- corrected %>%
  ggplot(aes(x = psi_wl, y = ap_corr)) +
  # geom_errorbar(aes(ymin = ap - se, ymax = ap + se), size = 2, width = 1) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = ap_corr - se_total_a, ymax = ap_corr - se_total_a), size = 6, width = 0.5) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Wavelength", y = expression(paste("a, m"^-1))) +
  theme_classic2(20)
```

``` r
b_plot <- corrected %>%
  ggplot(aes(x = psi_wl, y = b)) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0,0)) +
  ylim(c(0, 4)) +
  labs(x = "Wavelength", y = expression(paste("b, m"^-1))) +
  theme_classic2(20)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
c_plot <- corrected %>%
  ggplot(aes(x = psi_wl, y = cp_corr)) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = cp_corr - se_total_c, ymax = cp_corr - se_total_c), size = 6, width = 0.5) +
  scale_y_continuous(expand = c(0,0)) +
  ylim(c(0, 4)) +
  labs(x = "Wavelength", y = expression(paste("c, m"^-1))) +
  theme_classic2(20)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
(a_plot + inset_element(a_plot2, left = 0.3, bottom = 0.4, right = 1, top = 1, align_to = 'full') ) +
  
  b_plot + c_plot
```

    ## Warning: Removed 12 row(s) containing missing values (geom_path).

    ## Warning: Removed 1 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](acs_lab4_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->
