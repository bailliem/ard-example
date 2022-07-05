Example of reading & writing to ARD
================

This is an example of:

-   selecting (i.e. a query) from an analysis results database an
    endpoint from three clinical studies for the PBO arm,
-   generating a historical control,
-   writing the estimates from the MAP prior back to the ARD database.

## Set up

Load the required packages and path to a CSV file containing the
analysis dataset using a mock data model.

TODO: The model is not yet broken down by entity-relationship.

``` r
library(knitr)
#> Warning: package 'knitr' was built under R version 4.1.3
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.9
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.1     v forcats 0.5.1
#> Warning: package 'dplyr' was built under R version 4.1.3
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(RBesT)
#> Warning: package 'RBesT' was built under R version 4.1.3
#> This is RBesT version 1.6.3
library(RSQLite)  ## require an updated version fix column order bug when appending 
#> Warning: package 'RSQLite' was built under R version 4.1.3
library(DBI)
path_to_data <- "./data/ARD-example.csv"
ARD <- read.csv(path_to_data)
```

## Load dataset in to SQL database

Use RSQLite as a proof of concent.

Note: need to update the version of RSQlite to address an issue with
column ordering when appending back to the database.

First step, set up sqlite database.

``` r
mydb <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
dbDisconnect(mydb)
unlink("my-db.sqlite")
mydb <- dbConnect(RSQLite::SQLite(), "")
dbDisconnect(mydb)
```

Insert the analysis result data.frame as a table in the database. This
table has a lot of duplication that can be simplified by fully modelling
the entity-relationships.

``` r
mydb <- dbConnect(RSQLite::SQLite(), "")
dbWriteTable(mydb, "ARD", ARD)
```

Display the column names in the table as a sanity check.

``` r
dbListTables(mydb)
#> [1] "ARD"
```

Also display the first five rows of the new ARD table.

``` r
dbGetQuery(mydb, 'SELECT * FROM ARD LIMIT 5')
#>   UANALID STUDYID ANALID ANALFLG    TRTVAR TRTVAL SUBVAR SUBVAL AVISTN
#> 1     1.1       1      1     FAS treatment  COMBO     NA     NA    EOS
#> 2     1.1       1      1     FAS treatment  COMBO     NA     NA    EOS
#> 3     1.1       1      1     FAS treatment  COMBO     NA     NA    EOS
#> 4     1.2       1      2     FAS treatment    PBO     NA     NA    EOS
#> 5     1.2       1      2     FAS treatment    PBO     NA     NA    EOS
#>                                    PARAM PARAMCD ROWCAT1    ANALTYP1 ANALTYP2
#> 1 Primary composite outcome: CVD or FHFH     PCE      NA DESCRIPTIVE       NA
#> 2 Primary composite outcome: CVD or FHFH     PCE      NA DESCRIPTIVE       NA
#> 3 Primary composite outcome: CVD or FHFH     PCE      NA DESCRIPTIVE       NA
#> 4 Primary composite outcome: CVD or FHFH     PCE      NA DESCRIPTIVE       NA
#> 5 Primary composite outcome: CVD or FHFH     PCE      NA DESCRIPTIVE       NA
#>      STAT STATVAL   ANALMETH
#> 1       N  2340.0      count
#> 2       n   770.0      count
#> 3 percent    32.9 percentage
#> 4       N  2340.0      count
#> 5       n   791.0      count
#>                                                     ANALDESCRIPTION
#> 1             N: Total number of patients included in the analysis.
#> 2 n: Total number of patients with events included in the analysis.
#> 3                                                                  
#> 4             N: Total number of patients included in the analysis.
#> 5 n: Total number of patients with events included in the analysis.
```

## Query the database

Now we can query database.

We want to query for studies that have:

-   an outcome “PCE” which is short for “Primary composite endpoint”,
-   the analysis is “descriptive” (ANALYTYP1 = DESCRIPTIVE”“),
-   and a placebo treatment arm (TRTVAL = PBO),
-   return the selected data and write to dataframe,
-   and transform ready for RBesT,
-   display a glimpse of the df structure to ensure this worked.

``` r
df <- dbGetQuery(mydb, 'SELECT STUDYID, TRTVAL, STAT, STATVAL FROM ARD WHERE PARAMCD = "PCE" AND ANALTYP1 = "DESCRIPTIVE" AND TRTVAL = "PBO" ') %>%
  group_by(STUDYID, TRTVAL, STAT) %>%
   mutate(id = row_number()) %>% 
   gather(groupname, value, -id, -STUDYID, -TRTVAL, -STAT) %>%
   spread(STAT, value) %>%
   select(TRTVAL, N, n) %>%  
   glimpse()
#> Adding missing grouping variables: `STUDYID`
#> Rows: 3
#> Columns: 4
#> Groups: STUDYID, TRTVAL [3]
#> $ STUDYID <int> 1, 2, 3
#> $ TRTVAL  <chr> "PBO", "PBO", "PBO"
#> $ N       <dbl> 2340, 1800, 900
#> $ n       <dbl> 791, 400, 100
```

## MAP prior of three studies using RBesT

We run the placebo arm through RBest to generate a MAP prior. Note: we
are just using the default settings without any thought to make sure the
data is in the correct format.

``` r
set.seed(34563)
map_mcmc <- gMAP(cbind(n, N-n) ~ 1 | STUDYID,
                 data=df,
                 tau.dist="HalfNormal",
                 tau.prior=1,
                 beta.prior=2,
                 family=binomial)
#> Assuming default prior location   for beta: 0
```

Print the output of the model.

``` r
print(map_mcmc)
#> Generalized Meta Analytic Predictive Prior Analysis
#> 
#> Call:  gMAP(formula = cbind(n, N - n) ~ 1 | STUDYID, family = binomial, 
#>     data = df, tau.dist = "HalfNormal", tau.prior = 1, beta.prior = 2)
#> 
#> Exchangeability tau strata: 1 
#> Prediction tau stratum    : 1 
#> Maximal Rhat              : 1 
#> 
#> Between-trial heterogeneity of tau prediction stratum
#>  mean    sd  2.5%   50% 97.5% 
#> 0.881 0.395 0.377 0.792 1.920 
#> 
#> MAP Prior MCMC sample
#>   mean     sd   2.5%    50%  97.5% 
#> 0.2620 0.1820 0.0329 0.2210 0.7610
```

Check that a graphical representation of model checks is available

``` r
pl <- plot(map_mcmc)
```

Check that a number of plots are immediately defined

``` r
names(pl)
#> [1] "densityThetaStar"     "densityThetaStarLink" "forest_model"
```

Display the forest plot with model estimates

``` r
print(pl$forest_model)
```

![](ard-rbest-ard_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Arrange data and write back to the db

Now we need to arrange the data into a format that can be inserted back
in to the database.

To do so we:

-   Map the output to datatable ARD schema (NOTE: this is very manual at
    the moment),
-   gather the statistics from the three mcmc chains into seperate
    analyes
-   give each analysis its own unique id
-   indicate that the analysis comes from three rather than one study.

``` r
map <- as_tibble(map_mcmc$theta.strat) %>%
  mutate(
    ANALID = row_number(),
    STUDYID = "123",
    UANALID = paste0(STUDYID, ".", ANALID),
    ANALFLG = "FAS",
    TRTVAR = "treatment",
    TRTVAL = "PBO",
    SUBVAR = "",
    SUBVAL = "",
    AVISTN = "EOS",
    ANALMETH = "MAP prior",
    PARAM   = "Primary composite outcome: CVD or FHFH",
    PARAMCD = "PCE",
    ROWCAT1 = "ROWCAT1",
    ANALTYP1 = "PREDICTIVE",
    ANALTYP2 = "",
    ANALDESCRIPTION = "RBesT MAP Prior PBO from study 1, 2 and 3"
  ) %>%
  gather(
    "STAT",
    "STATVAL",-UANALID,
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
  glimpse()
#> Rows: 12
#> Columns: 18
#> $ ANALID          <int> 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3
#> $ STUDYID         <chr> "123", "123", "123", "123", "123", "123", "123", "123"~
#> $ UANALID         <chr> "123.1", "123.2", "123.3", "123.1", "123.2", "123.3", ~
#> $ ANALFLG         <chr> "FAS", "FAS", "FAS", "FAS", "FAS", "FAS", "FAS", "FAS"~
#> $ TRTVAR          <chr> "treatment", "treatment", "treatment", "treatment", "t~
#> $ TRTVAL          <chr> "PBO", "PBO", "PBO", "PBO", "PBO", "PBO", "PBO", "PBO"~
#> $ SUBVAR          <chr> "", "", "", "", "", "", "", "", "", "", "", ""
#> $ SUBVAL          <chr> "", "", "", "", "", "", "", "", "", "", "", ""
#> $ AVISTN          <chr> "EOS", "EOS", "EOS", "EOS", "EOS", "EOS", "EOS", "EOS"~
#> $ ANALMETH        <chr> "MAP prior", "MAP prior", "MAP prior", "MAP prior", "M~
#> $ PARAM           <chr> "Primary composite outcome: CVD or FHFH", "Primary com~
#> $ PARAMCD         <chr> "PCE", "PCE", "PCE", "PCE", "PCE", "PCE", "PCE", "PCE"~
#> $ ROWCAT1         <chr> "ROWCAT1", "ROWCAT1", "ROWCAT1", "ROWCAT1", "ROWCAT1",~
#> $ ANALTYP1        <chr> "PREDICTIVE", "PREDICTIVE", "PREDICTIVE", "PREDICTIVE"~
#> $ ANALTYP2        <chr> "", "", "", "", "", "", "", "", "", "", "", ""
#> $ ANALDESCRIPTION <chr> "RBesT MAP Prior PBO from study 1, 2 and 3", "RBesT MA~
#> $ STAT            <chr> "mean", "mean", "mean", "se", "se", "se", "2.5%", "2.5~
#> $ STATVAL         <dbl> -0.6720669, -1.2527630, -2.0794415, -4.6177009, -4.615~
```

Check that the data.frame maps to the ARD schema by displaying the
columns in table ARD.

``` r
dbListFields(mydb, "ARD") 
#>  [1] "UANALID"         "STUDYID"         "ANALID"          "ANALFLG"        
#>  [5] "TRTVAR"          "TRTVAL"          "SUBVAR"          "SUBVAL"         
#>  [9] "AVISTN"          "PARAM"           "PARAMCD"         "ROWCAT1"        
#> [13] "ANALTYP1"        "ANALTYP2"        "STAT"            "STATVAL"        
#> [17] "ANALMETH"        "ANALDESCRIPTION"
```

## Append to database

Now we write back to the database appending to the table.

``` r
dbWriteTable(mydb, "ARD", map, append = TRUE)
```

We then check that the data from the new analysis is there.

``` r
dbGetQuery(mydb, 'SELECT * FROM ARD WHERE PARAMCD = "PCE" AND ANALTYP1 = "PREDICTIVE" AND TRTVAL = "PBO" ')
#>    UANALID STUDYID ANALID ANALFLG    TRTVAR TRTVAL SUBVAR SUBVAL AVISTN
#> 1    123.1     123      1     FAS treatment    PBO                  EOS
#> 2    123.2     123      2     FAS treatment    PBO                  EOS
#> 3    123.3     123      3     FAS treatment    PBO                  EOS
#> 4    123.1     123      1     FAS treatment    PBO                  EOS
#> 5    123.2     123      2     FAS treatment    PBO                  EOS
#> 6    123.3     123      3     FAS treatment    PBO                  EOS
#> 7    123.1     123      1     FAS treatment    PBO                  EOS
#> 8    123.2     123      2     FAS treatment    PBO                  EOS
#> 9    123.3     123      3     FAS treatment    PBO                  EOS
#> 10   123.1     123      1     FAS treatment    PBO                  EOS
#> 11   123.2     123      2     FAS treatment    PBO                  EOS
#> 12   123.3     123      3     FAS treatment    PBO                  EOS
#>                                     PARAM PARAMCD ROWCAT1   ANALTYP1 ANALTYP2
#> 1  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 2  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 3  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 4  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 5  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 6  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 7  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 8  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 9  Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 10 Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 11 Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#> 12 Primary composite outcome: CVD or FHFH     PCE ROWCAT1 PREDICTIVE         
#>     STAT    STATVAL  ANALMETH                           ANALDESCRIPTION
#> 1   mean -0.6720669 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 2   mean -1.2527630 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 3   mean -2.0794415 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 4     se -4.6177009 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 5     se -4.6156195 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 6     se -4.5481703 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 7   2.5% -0.7589839 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 8   2.5% -1.3663904 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 9   2.5% -2.2975852 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 10 97.5% -0.5857623 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 11 97.5% -1.1409261 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
#> 12 97.5% -1.8704945 MAP prior RBesT MAP Prior PBO from study 1, 2 and 3
```

Disconnect the database.

``` r
dbDisconnect(mydb)
```

## Session info

``` r
sessionInfo()
#> R version 4.1.2 (2021-11-01)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19042)
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.1252 
#> [2] LC_CTYPE=English_United States.1252   
#> [3] LC_MONETARY=English_United States.1252
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] DBI_1.1.2       RSQLite_2.2.11  RBesT_1.6-3     forcats_0.5.1  
#>  [5] stringr_1.4.0   dplyr_1.0.9     purrr_0.3.4     readr_2.1.1    
#>  [9] tidyr_1.1.4     tibble_3.1.6    ggplot2_3.3.5   tidyverse_1.3.1
#> [13] knitr_1.38     
#> 
#> loaded via a namespace (and not attached):
#>  [1] matrixStats_0.61.0   fs_1.5.2             lubridate_1.8.0     
#>  [4] bit64_4.0.5          httr_1.4.2           rstan_2.21.5        
#>  [7] tensorA_0.36.2       tools_4.1.2          backports_1.4.1     
#> [10] utf8_1.2.2           R6_2.5.1             colorspace_2.0-2    
#> [13] withr_2.4.3          tidyselect_1.1.1     gridExtra_2.3       
#> [16] prettyunits_1.1.1    processx_3.5.2       bit_4.0.4           
#> [19] compiler_4.1.2       cli_3.3.0            rvest_1.0.2         
#> [22] xml2_1.3.2           labeling_0.4.2       posterior_1.2.2     
#> [25] scales_1.1.1         checkmate_2.0.0      mvtnorm_1.1-3       
#> [28] ggridges_0.5.3       callr_3.7.0          digest_0.6.28       
#> [31] StanHeaders_2.21.0-7 rmarkdown_2.11       pkgconfig_2.0.3     
#> [34] htmltools_0.5.2      highr_0.9            dbplyr_2.1.1        
#> [37] fastmap_1.1.0        rlang_1.0.2          readxl_1.3.1        
#> [40] rstudioapi_0.13      farver_2.1.0         generics_0.1.1      
#> [43] jsonlite_1.7.2       distributional_0.3.0 inline_0.3.19       
#> [46] magrittr_2.0.3       Formula_1.2-4        loo_2.5.1           
#> [49] bayesplot_1.9.0      Rcpp_1.0.7           munsell_0.5.0       
#> [52] fansi_0.5.0          abind_1.4-5          lifecycle_1.0.1     
#> [55] stringi_1.7.5        yaml_2.2.1           pkgbuild_1.3.1      
#> [58] plyr_1.8.6           grid_4.1.2           blob_1.2.2          
#> [61] parallel_4.1.2       crayon_1.4.2         haven_2.4.3         
#> [64] hms_1.1.1            ps_1.6.0             pillar_1.6.4        
#> [67] reshape2_1.4.4       codetools_0.2-18     stats4_4.1.2        
#> [70] rstantools_2.2.0     reprex_2.0.1         glue_1.6.2          
#> [73] evaluate_0.15        RcppParallel_5.1.5   modelr_0.1.8        
#> [76] vctrs_0.4.1          tzdb_0.2.0           cellranger_1.1.0    
#> [79] gtable_0.3.0         assertthat_0.2.1     cachem_1.0.6        
#> [82] xfun_0.30            broom_0.8.0          memoise_2.0.0       
#> [85] ellipsis_0.3.2
```
