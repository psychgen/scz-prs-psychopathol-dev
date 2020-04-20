Analysis code overview: scz\_prs\_general
================
laurie
26 February 2020

-   [Introduction](#introduction)
-   [Data preparation](#data-preparation)
    -   [Scale-level data prepartion](#scale-level-data-prepartion)
    -   [Item-level data preparation](#item-level-data-preparation)
    -   [Creating polygenic scores](#creating-polygenic-scores)
    -   [Aggregation of processed data and polygenic scores](#aggregation-of-processed-data-and-polygenic-scores)
-   [Analyses](#analyses)
    -   [Growth modelling (CBCL internalizing/externalizing)](#growth-modelling-cbcl-internalizingexternalizing)
    -   ["p" factor modelling](#p-factor-modelling)
    -   [Latent profile analysis](#latent-profile-analysis)
    -   ["p" factor modelling](#p-factor-modelling-1)
    -   [Latent profile analysis](#latent-profile-analysis-1)

Introduction
============

This is an analysis code overview document, written in RMarkdown, for the project entitled [Genetic liability for schizophrenia and childhood psychopathology in the Norwegian Mother, Father, and Child Cohort study (MoBa)](add_preprint_link). In it, we walk through the scripts used to complete the analyses for this project.

Data preparation
================

There are three main analytic components to this project:

1.  Growth modelling, sub-scale scores from 3 waves of CBCL internalizing/externalizing data
2.  "p" factor modelling, item-level data from RS-DBD, sMFQ, SCARED at 8 years
3.  Latent profile analysis, using scale scores from (1) and compiled scale scores from (2)

Scale-level data prepartion
---------------------------

First, to prepare data on the scale level, we use the `00_prep_scales.R` script. This script:

-   Reads in SPSS files with MoBa phenotypic data, selecting variables corresponding to items from relevant scales only
-   Stores the names of analysis-relevant items (i.e., only wave-to-wave consistent CBCL items selected )for each scale in named objects
-   Gathers data to long format, recodes item responses to numeric values, groups by scale (and wave, where appropriate), using the following code (example for CBCL variables):

``` r
#Re-shape to long and recode items (and assign to subscales)

q565yr <- q5sel %>% 
  full_join(q6sel) %>%
  full_join(q5yrsel) %>%
  gather(item,val,-PREG_ID_2306, -BARN_NR) %>%
  mutate(true_val = ifelse(val %in% c("Not true","Rarely/never"),0,
                           ifelse(val %in% c("Somewhat or sometimes true","Sometimes"),1,
                                  ifelse(val %in% c("Very true or often true","Often/ typical"),2, NA))),
         age = ifelse(str_detect(item, "EE"),"18mo",
                      ifelse(str_detect(item, "GG"),"3yr",
                             ifelse(str_detect(item, "LL"),"5yr",NA))),
         sscale = ifelse(item %in% int_growth,"cbcl_int_growth",
                          ifelse(item %in% ext_growth,"cbcl_ext_growth",NA))) %>% 
  drop_na(sscale)

head(q565yr[,-1])
 # BARN_NR  item                        val true_val  age          sscale
 #       1 EE908                   Not true        0 18mo cbcl_int_growth
 #       1 EE908                   Not true        0 18mo cbcl_int_growth
 #       1 EE908                   Not true        0 18mo cbcl_int_growth
 #       1 EE908                   Not true        0 18mo cbcl_int_growth
 #       1 EE908                       <NA>       NA 18mo cbcl_int_growth
 #       1 EE908 Somewhat or sometimes true        1 18mo cbcl_int_growth
```

-   Computes scale scores by multiplying the mean of all available items in a scale by the total number of items in the scale, as long as more than half the items are non-missing:

``` r
#Summarize to get components of scale score computation

q565yr_scale <- q565yr %>% 
  group_by( age, sscale) %>%
  summarize(items_scale = length(unique(item))) %>%
  right_join(q565yr) %>%
  group_by(PREG_ID_2306,BARN_NR,  age, sscale, items_scale) %>%
  summarize(items_present = sum(!is.na(val)),
            score = mean(true_val, na.rm=T)) %>%
  ungroup()

#Compute scale score

q565yr_scale <- q565yr_scale %>%
  mutate(sc_score = ifelse(items_present >= (items_scale/2), round(score*items_scale,0), NA))

#Back to wide

 cbcl565yr <- q565yr_scale %>% 
  unite(age_sscale,  sscale, age) %>%
  select(-c(items_scale, items_present, score)) %>%
  spread(key=age_sscale, value = sc_score) 
 cor(cbcl565yr[,-c(1:2)], use = "pairwise.complete.obs")
 
 
 head(cbcl565yr[,-1])
#    BARN_NR cbcl_ext_growth_18mo cbcl_ext_growth_3yr cbcl_ext_growth_5yr cbcl_int_growth_18mo cbcl_int_growth_3yr cbcl_int_growth_5yr
#     <dbl>                <dbl>               <dbl>               <dbl>                <dbl>               <dbl>               <dbl>
# 1       1                    7                  NA                   1                    0                  NA                   1
# 2       1                    6                   4                   5                    3                   2                   0
# 3       1                    5                   5                   1                    4                   3                   1
# 4       1                    1                   0                  NA                    1                   1                  NA
# 5       1                    0                  NA                  NA                   NA                  NA                  NA
# 6       1                    8                  NA                  NA                    5                  NA                  NA
```

Item-level data preparation
---------------------------

The item-level data for analysis (2) is prepared in the script `00d_prep_items.R` in the same manner.

Creating polygenic scores
-------------------------

Polygenic scores were created using PRSice2, run on a high performance computing cluster via the shell script `off_scz.sh`. These are processed (i.e., regressed on genotyping batch and 10 principal components) in R using the script `XX_process_all_scores.R`.

Aggregation of processed data and polygenic scores
--------------------------------------------------

The scale-level data and item-level data are joined with the polygenic scores in the script `00b_aggregate_scales_prs.R`, such that the datafiles used in the scale-level analyses is `all_scales_PRS.txt` and `all_items_PRS.txt`. These are found in the `./scratch_data` folder.

Analyses
========

Growth modelling (CBCL internalizing/externalizing)
---------------------------------------------------

The latent growth models are run using the script `01_growth_modelling.R`, which sources scripts `01.1...` and `01.2` for, respectively, specifying and running the growth models. Models are run using the `lavaan` package, and path diagrams generated using the `semPlot` package, as per the example below:

``` r
fit1 <- growth(model1, data=dat, missing="fiml.x")

semPaths(fit1, style = 'ram', intercepts = F)

print(fit1)
```

After running the models, the script cleans the output and then performs the model fitting procedure as specified in the manuscript. This takes us from the raw output to a dataframe showing the best fitting model at each PRS threshold for each variable.

Parameter estimates for the effect of PRS on the latent growth process are then extracted and plotted in the remainder of the script.

"p" factor modelling
--------------------

The 8-year "p" factor models are run on item-level data using the script `02_pfact_modelling.R`. The models are specified in the sourced script `02.1_specify_pfact_models.R` and run using lavaan with a call to the function created in `02.2_run_pfact_models.R` as above.

Latent profile analysis
-----------------------

The latent profile analysis is set-up and managed using the script `02_pfact_modelling.R`, with help from the `MplusAutomation` package. The mplus scripts (or .inp files) are created manually external to R (although MplusAutomation does allow the creation of input files from within R). These files are located in `scripts/mplus/lpa`. There is a single external call to Mplus to run the models (line 38: `runModels(filepath1, logFile="allGMM.txt", recursive =T,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )`), followed by some file management steps. The output and summaries from the models are then read in to R using the `readModels()` function.

LPA model fit statistics and class counts are extracted from the summaries object, and the odds ratios for the effect of PRS on profile assignment probability are then somewhat clumsily pulled from a text read-in of the output files for each threshold in a loop:

``` r
### ORS from 4class LPA at all thresholds

models <- c("4c_0001","4c_001","4c_01","4c_05","4c_10","4c_20","4c_50","4c")
res <- data.frame()
for(i in models){
  
  b<- readLines(paste0("./output/lpa/lpa_",i,".out"))
  
  c <- b[grep("X0_0001_|X0_001_|X0_01_|X0_05_|X0_1_|X0_2_|X0_5_|X1_",b)] %>% 
    as.data.frame() %>% 
    `colnames<-`(c("var")) %>%
    mutate(var = gsub("\\s+", " ", str_trim(var))) %>%
    separate(var, into= c("var", "Lowerp5%", "lci","Lower5%","est","Upper5%","uci","Upperp5%"), sep= " ") %>% 
    drop_na("Upperp5%") %>% 
    select( est, lci,uci) %>%
    mutate(model=i,
           comp = c(rep("drop",4), 
                    "Ref4_C1",
                    "Ref4_C2",
                    "Ref4_C3",
                    rep("drop",9), 
                    "Ref1_C2",
                    "Ref1_C3",
                    "Ref1_C4",
                    "Ref2_C1",
                    "Ref2_C3",
                    "Ref2_C4",
                    "Ref3_C1",
                    "Ref3_C2",
                    "Ref3_C4")) %>% 
    filter(!comp == "drop") 
  
  res <- bind_rows(res,c)
}

write.table(res, "./output/LPA_ORs.txt", row.names = F, quote = F)
```

Parameter estimates for the effect of PRS on the latent growth process are then extracted and plotted in the remainder of the script.

"p" factor modelling
--------------------

The 8-year "p" factor models are run on item-level data using the script `02_pfact_modelling.R`. The models are specified in the sourced script `02.1_specify_pfact_models.R` and run using lavaan with a call to the function created in `02.2_run_pfact_models.R` as above.

Latent profile analysis
-----------------------

The latent profile analysis is set-up and managed using the script `02_pfact_modelling.R`, with help from the `MplusAutomation` package. The mplus scripts (or .inp files) are created manually external to R (although MplusAutomation does allow the creation of input files from within R). These files are located in `scripts/mplus/lpa`. There is a single external call to Mplus to run the models (line 38: `runModels(filepath1, logFile="allGMM.txt", recursive =T,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )`), followed by some file management steps. The output and summaries from the models are then read in to R using the `readModels()` function.

LPA model fit statistics and class counts are extracted from the summaries object, and the odds ratios for the effect of PRS on profile assignment probability are then somewhat clumsily pulled from a text read-in of the output files for each threshold in a loop:

``` r
### ORS from 4class LPA at all thresholds

models <- c("4c_0001","4c_001","4c_01","4c_05","4c_10","4c_20","4c_50","4c")
res <- data.frame()
for(i in models){
  
  b<- readLines(paste0("./output/lpa/lpa_",i,".out"))
  
  c <- b[grep("X0_0001_|X0_001_|X0_01_|X0_05_|X0_1_|X0_2_|X0_5_|X1_",b)] %>% 
    as.data.frame() %>% 
    `colnames<-`(c("var")) %>%
    mutate(var = gsub("\\s+", " ", str_trim(var))) %>%
    separate(var, into= c("var", "Lowerp5%", "lci","Lower5%","est","Upper5%","uci","Upperp5%"), sep= " ") %>% 
    drop_na("Upperp5%") %>% 
    select( est, lci,uci) %>%
    mutate(model=i,
           comp = c(rep("drop",4), 
                    "Ref4_C1",
                    "Ref4_C2",
                    "Ref4_C3",
                    rep("drop",9), 
                    "Ref1_C2",
                    "Ref1_C3",
                    "Ref1_C4",
                    "Ref2_C1",
                    "Ref2_C3",
                    "Ref2_C4",
                    "Ref3_C1",
                    "Ref3_C2",
                    "Ref3_C4")) %>% 
    filter(!comp == "drop") 
  
  res <- bind_rows(res,c)
}

write.table(res, "./output/LPA_ORs.txt", row.names = F, quote = F)
```

A selection of these ORs are then plotted, before the `adapted_functions.R` script is sourced to facilitate plotting of the trajectories from the growth part of the model. The final plot is for the profile-specific estimated means on each of the six 8-year scales.
