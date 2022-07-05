Query CDISC ADAE
================

``` r
library(tidyverse)
#> ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.1.0     ✔ purrr   0.3.2
#> ✔ tibble  2.1.3     ✔ dplyr   0.8.3
#> ✔ tidyr   0.8.0     ✔ stringr 1.3.0
#> ✔ readr   1.1.1     ✔ forcats 0.3.0
#> ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(here)
#> here() starts at /home/baillma3/R/ards
library(RSQLite)
library(dbplyr)  ## may need to update dbplyr 
#> 
#> Attaching package: 'dbplyr'
#> The following objects are masked from 'package:dplyr':
#> 
#>     ident, sql
library(DBI)
```

Load example CDISC data from
<https://github.com/phuse-org/phuse-scripts>.

``` r
adsl <- haven::read_xpt(here("data","adsl.xpt"))
adae <- haven::read_xpt(here("data","adae.xpt"))
```

Calculate the number of patients by treatment in the safety population
(SAFFL). NOTE: All of these steps to generate summary statistics could
be performed within a database using SQL.

``` r
bigN <- adae %>%
  filter(SAFFL == "Y") %>% ## SAF population
  group_by(TRTA) %>%
  summarise(N = n()) %>%
  mutate(N = N)
```

Calculate the number of patients by treatment assigned by adverse event
(dictionary dervived term).

``` r
smalln <- adae %>% 
  filter(SAFFL == "Y") %>% ## SAF population
  group_by(AEDECOD, TRTA) %>% # group by PT - disctionary dervived term and trt
  summarise(n = n()) %>%
  mutate(n = n) 
```

Finally, calculate the percentage of events by derived term i.e. the
treatment emergent adverse events.

``` r
teae <- bigN %>%
  left_join(smalln) %>%
  mutate(inc = n / N, 
         per = n / N * 100) %>%
  arrange(TRTA, AEDECOD) %>% 
  glimpse()
#> Joining, by = "TRTA"
#> Observations: 373
#> Variables: 6
#> $ TRTA    <chr> "Placebo", "Placebo", "Placebo", "Placebo", "Placebo", "…
#> $ N       <int> 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 3…
#> $ AEDECOD <chr> "ABDOMINAL PAIN", "AGITATION", "ALOPECIA", "ANXIETY", "A…
#> $ n       <int> 1, 2, 1, 2, 9, 3, 1, 7, 10, 2, 2, 1, 2, 2, 1, 2, 1, 2, 2…
#> $ inc     <dbl> 0.003322259, 0.006644518, 0.003322259, 0.006644518, 0.02…
#> $ per     <dbl> 0.3322259, 0.6644518, 0.3322259, 0.6644518, 2.9900332, 0…
```

## Database

``` r
ARD <- teae %>%
  mutate(
    ANALID = row_number(),
    STUDYID = "1",
    UANALID = paste0(STUDYID, ".", ANALID),
    ANALFLG = "SAFFL",
    TRTVAR = "TRTA",
    TRTVAL = TRTA,
    SUBVAR = "",
    SUBVAL = "",
    AVISTN = "EOS",
    ANALMETH = "Summary statistics",
    PARAM   = "treatment emergent adverse event (dictionary dervived term)",
    PARAMCD = "TEAE" ,
    ROWCAT1 = AEDECOD,
    ANALTYP1 = "DESCRIPTIVE",
    ANALTYP2 = "",
    ANALDESCRIPTION = "Descriptive summary of treatment emergent adverse events by treatment."
  ) %>%
  select(-AEDECOD, -TRTA) %>%
  gather(
    "STAT",
    "STATVAL",
    -UANALID,
    -STUDYID,
    -ANALID,
    -ANALFLG,
    -TRTVAR,
    -TRTVAL,
    -SUBVAR,
    -SUBVAL,
    -AVISTN,
    -PARAM,
    -PARAMCD,
    -ROWCAT1,
    -ANALTYP1,
    -ANALTYP2,
    -ANALMETH,
    -ANALDESCRIPTION
  ) %>%
  arrange(STUDYID, ANALID, STAT) %>%
  glimpse()
#> Observations: 1,492
#> Variables: 18
#> $ ANALID          <int> 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, …
#> $ STUDYID         <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"…
#> $ UANALID         <chr> "1.1", "1.1", "1.1", "1.1", "1.2", "1.2", "1.2",…
#> $ ANALFLG         <chr> "SAFFL", "SAFFL", "SAFFL", "SAFFL", "SAFFL", "SA…
#> $ TRTVAR          <chr> "TRTA", "TRTA", "TRTA", "TRTA", "TRTA", "TRTA", …
#> $ TRTVAL          <chr> "Placebo", "Placebo", "Placebo", "Placebo", "Pla…
#> $ SUBVAR          <chr> "", "", "", "", "", "", "", "", "", "", "", "", …
#> $ SUBVAL          <chr> "", "", "", "", "", "", "", "", "", "", "", "", …
#> $ AVISTN          <chr> "EOS", "EOS", "EOS", "EOS", "EOS", "EOS", "EOS",…
#> $ ANALMETH        <chr> "Summary statistics", "Summary statistics", "Sum…
#> $ PARAM           <chr> "treatment emergent adverse event (dictionary de…
#> $ PARAMCD         <chr> "TEAE", "TEAE", "TEAE", "TEAE", "TEAE", "TEAE", …
#> $ ROWCAT1         <chr> "ABDOMINAL PAIN", "ABDOMINAL PAIN", "ABDOMINAL P…
#> $ ANALTYP1        <chr> "DESCRIPTIVE", "DESCRIPTIVE", "DESCRIPTIVE", "DE…
#> $ ANALTYP2        <chr> "", "", "", "", "", "", "", "", "", "", "", "", …
#> $ ANALDESCRIPTION <chr> "Descriptive summary of treatment emergent adver…
#> $ STAT            <chr> "inc", "n", "N", "per", "inc", "n", "N", "per", …
#> $ STATVAL         <dbl> 3.322259e-03, 1.000000e+00, 3.010000e+02, 3.3222…
```

Write to a database the results.

``` r
mydb <- dbConnect(RSQLite::SQLite(), "")
dbWriteTable(mydb, "ARD", ARD)
```

``` r
dbListTables(mydb)
#> [1] "ARD"
```

``` r
dbListFields(mydb, "ARD") 
#>  [1] "ANALID"          "STUDYID"         "UANALID"        
#>  [4] "ANALFLG"         "TRTVAR"          "TRTVAL"         
#>  [7] "SUBVAR"          "SUBVAL"          "AVISTN"         
#> [10] "ANALMETH"        "PARAM"           "PARAMCD"        
#> [13] "ROWCAT1"         "ANALTYP1"        "ANALTYP2"       
#> [16] "ANALDESCRIPTION" "STAT"            "STATVAL"
```

Query the database for AEs with an incidence over 5%.

``` r
dbdata <- tbl(mydb, "ARD")
dbdata %>%
  filter(ANALTYP1 == "DESCRIPTIVE" &  PARAMCD == "TEAE" & STAT == "inc" && STATVAL > 0.05 ) %>%
  select(ANALID, TRTVAL, ROWCAT1, STAT, STATVAL) 
#> # Source:   lazy query [?? x 5]
#> # Database: sqlite 3.29.0 []
#>   ANALID TRTVAL               ROWCAT1                   STAT  STATVAL
#>    <int> <chr>                <chr>                     <chr>   <dbl>
#> 1    131 Xanomeline High Dose APPLICATION SITE ERYTHEMA inc    0.0505
#> 2    135 Xanomeline High Dose APPLICATION SITE PRURITUS inc    0.0769
#> 3    218 Xanomeline High Dose PRURITUS                  inc    0.0835
#> 4    259 Xanomeline Low Dose  APPLICATION SITE PRURITUS inc    0.0759
#> 5    300 Xanomeline Low Dose  ERYTHEMA                  inc    0.0552
#> 6    338 Xanomeline Low Dose  PRURITUS                  inc    0.0805
```

Disconnect the database.

``` r
dbDisconnect(mydb)
```

## Session info

``` r
sessionInfo()
#> R version 3.4.3 (2017-11-30)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: OpenShift Enterprise
#> 
#> Matrix products: default
#> BLAS/LAPACK: /CHBS/apps/busdev_apps/eb/software/imkl/2018.1.163-GCC-6.4.0-2.28/compilers_and_libraries_2018.1.163/linux/mkl/lib/intel64_lin/libmkl_gf_lp64.so
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] DBI_1.0.0       dbplyr_1.4.2    RSQLite_2.1.2   here_0.1       
#>  [5] forcats_0.3.0   stringr_1.3.0   dplyr_0.8.3     purrr_0.3.2    
#>  [9] readr_1.1.1     tidyr_0.8.0     tibble_2.1.3    ggplot2_3.1.0  
#> [13] tidyverse_1.2.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyselect_0.2.5     reshape2_1.4.3       haven_2.1.1         
#>  [4] lattice_0.20-35      vctrs_0.2.0          colorspace_1.3-2    
#>  [7] htmltools_0.3.6      yaml_2.1.19          utf8_1.1.3          
#> [10] blob_1.2.0           rlang_0.4.0          pillar_1.4.2        
#> [13] foreign_0.8-69       glue_1.3.1           withr_2.1.2         
#> [16] bit64_0.9-7          modelr_0.1.1         readxl_1.0.0        
#> [19] plyr_1.8.4           munsell_0.5.0        gtable_0.2.0        
#> [22] cellranger_1.1.0     rvest_0.3.2          memoise_1.1.0       
#> [25] psych_1.7.8          evaluate_0.10.1      knitr_1.20          
#> [28] BiocInstaller_1.28.0 parallel_3.4.3       fansi_0.4.0         
#> [31] broom_0.4.3          Rcpp_1.0.2           scales_1.0.0        
#> [34] backports_1.1.2      jsonlite_1.5         bit_1.1-14          
#> [37] mnormt_1.5-5         hms_0.4.2            digest_0.6.15       
#> [40] stringi_1.1.7        grid_3.4.3           rprojroot_1.3-2     
#> [43] cli_1.0.0            tools_3.4.3          magrittr_1.5        
#> [46] lazyeval_0.2.1       zeallot_0.1.0        crayon_1.3.4        
#> [49] pkgconfig_2.0.1      xml2_1.2.0           lubridate_1.7.3     
#> [52] assertthat_0.2.0     rmarkdown_1.11       httr_1.3.1          
#> [55] rstudioapi_0.7       R6_2.2.2             nlme_3.1-131.1      
#> [58] compiler_3.4.3
```
