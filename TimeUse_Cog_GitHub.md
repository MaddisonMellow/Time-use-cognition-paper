Twenty-four-hour time-use composition and cognitive function in older
adults: Cross-sectional findings of the ACTIVate study
================
Maddison Mellow
2022-11-28

## Background

Physical activity, sedentary behaviour and sleep are associated with
cognitive function in older adults. However, these behaviours are not
independent, but instead make up exclusive and exhaustive components of
the 24-hour day. Few studies have investigated associations between
24-hour time-use composition and cognitive function in older adults. Of
these, none have considered how the quality of sleep, or the context of
physical activity and sedentary behaviour may impact these
relationships. This study aimed to understand how 24-hour time-use
composition is associated with cognitive function across a range of
domains in healthy older adults, and whether the level of recreational
physical activity, amount of television (TV) watching, or the quality of
sleep impact these potential associations.

The following analysis pipelines were used for our study, “24-hour
time-use composition and cognitive function in older adults:
cross-sectional findings of the ACTIVate study”
(<https://www.frontiersin.org/articles/10.3389/fnhum.2022.1051793/full>).

## Data overview

The code presented here was replicated for each cognitive outcome
(global cognition, long-term memory, short-term memory, executive
function and processing speed). Only the code used for the global
cognition outcome is presented here, for simplicity.

The following variables were included in analyses:

**Cognitive outcome measure**

-   `ace_score` = measure of global cognition (Addenbrooke’s Cognitive
    Examination III total score)

**Time-use variables**

-   `all_days_sleeptime` = total time spent in sleep per day (averaged
    over recording period)
-   `all_days_sedtime` = total time spent in sedentary behaviour per day
    (averaged over recording period)
-   `all_days_mvtime` = total time spent in moderate-vigorous physical
    activity per day (averaged over recording period)
-   `all_days_lighttime` = total time spent in light physical activity
    per day (averaged over recording period)

**Covariates**

-   `visit_age` (age in years at baseline visit)
-   `sex` = 2 levels (male or female)
-   `site_ch` = site, 2 levels (Adelaide or Newcastle)
-   `smoke_stat_ch` = smoking status, 3 levels (current, previous or
    never)
-   `edu_yrs` = total years education (primary + secondary + tertiary)

**Interaction terms**

-   `recpa_ch` = time spent in recreational physical activity per day, 3
    levels (none, 0-30 minutes/day, \>30 minutes/day)
-   `tvtime_ch` = time spent watching TV per day, 3 levels (low, medium,
    high tertiles)
-   `sleepqual_ch` = subjective sleep quality, 2 levels (good, bad)

## Code

#### 1. Load required packages

``` r
library("dplyr")
library("tidyr")
library("readr")
library("ggplot2")
library("lubridate")
library("compositions") 
library("skimr") 
library("GGally") 
library("rstatix")
```

#### 2. Load and review dataset

``` r
load('data/FinalData/final_dat.RData', verbose = TRUE)
skim(final_dat)
```

#### 3. Clean and refine dataset

Check that time-use data add up to \~1440 minutes (24-hr day)

``` r
rowSums(final_dat[, 
                  c("all_days_sleeptime", "all_days_sedtime", "all_days_lighttime", "all_days_mvtime")
])
```

Look for any invalid files (coded as NA or 0, as 1=valid)

``` r
table(final_dat$valid_file, useNA = 'ifany')
final_dat <- final_dat[final_dat$valid_file %in% 1, ] #remove participants that don't have valid accelerometry files
table(final_dat$valid_file, useNA = 'ifany') #check that they were removed 
```

Create new dataframes containing the outcome, predictors and covariates.

``` r
cols_outcome <- c("ace_score")
cols_pred <- c("all_days_sleeptime", "all_days_sedtime", "all_days_lighttime", "all_days_mvtime")
cols_covar <- c("visit_age", "sex", "site_ch", "smoke_stat_ch", "edu_yrs", "recpa_ch", "tvtime_ch", "sleepqual_ch")
```

Combine `cols_outcome`, `cols_covar` and `cols_pred` in to `cols_want`
dataframe

``` r
cols_want <- c(cols_outcome, cols_pred, cols_covar)
```

Create new dataframe ‘`act_dat`’ which contains all rows from
`final_dat` but only `cols_want` columns

``` r
act_dat <- final_dat[, cols_want]
```

Create new vector `rs_ad0`, then create new data frame `index_dont_want`
which only includes NAs from `rs_ad0` dataframe

``` r
rs_ad0 <- rowSums(act_dat[, cols_want %in% cols_pred]) 
index_dont_want <- is.na(rs_ad0)
table(index_dont_want) #displays the data in index_dont_want
act_dat <- act_dat[!index_dont_want, ] #update act_dat to exclude any variables from index_dont_want
nrow(act_dat)
```

Remove all NA values from `act_dat`

``` r
act_dat <- na.omit(act_dat) 
```

Check that time use data = \~1440 minutes

``` r
hist(rowSums(act_dat[, cols_pred])) #create histogram of all rows but only predictor variable columns
act_dat[rowSums(act_dat[, cols_pred]) > 1500, ] #show which rows add up to >1500 mins
act_dat[rowSums(act_dat[, cols_pred]) < 1400, ] #show which rows add up to <1400 mins

# locate which participants have >1500 mins time use (for data exploration purposes)
final_dat[rowSums(final_dat[, cols_pred]) > 1500, ]

# check that wear time is equal to non-sleeping time (columns should add up to same amount)
cbind(
  rowSums(final_dat[, c("all_days_sedtime", "all_days_lighttime", "all_days_mvtime")]),
  final_dat[,"all_days_weartime" ]
)

# calculate difference in wear time and total waking activity
sort(
  rowSums(final_dat[, c("all_days_sedtime", "all_days_lighttime", "all_days_mvtime")]) -
    final_dat[["all_days_weartime" ]]
) 

# Check if there are any NA values in wear time column
table(is.na(final_dat$all_days_weartime))
hist(final_dat$all_days_weartime)
```

Remove participants with more than 1500 minutes of time use data
\*\*\*note that this was an arbitrary but pragmatic decision to remove
observations with potentially misleading time-use recordings made by
researchers for this study.

``` r
nrow(act_dat)
act_dat <- act_dat[!(rowSums(act_dat[, cols_pred]) > 1500), ]
nrow(act_dat)
```

Apply closure function (`compositions` package), which rescales time-use
data so that all participants have 1440 mins time use

``` r
act_dat[, cols_pred] <- clo(act_dat[, cols_pred], total = 1440)

rowSums(act_dat[, cols_pred]) #checks that all participants have 1440 mins
```

Check that factor variables are classified correctly

``` r
str(act_dat)
act_dat$sex <- factor(act_dat$sex)
act_dat$site_ch=factor(act_dat$site_ch)
act_dat$smoke_stat_ch=factor(act_dat$smoke_stat_ch)
act_dat$sleepqual_ch=factor(act_dat$sleepqual_ch)
act_dat$tvtime_ch=factor(act_dat$tvtime_ch)
act_dat$recpa_ch=factor(act_dat$recpa_ch)
str(act_dat)
```

#### 4. Correlations between independent time-use behaviours and cognitive outcomes

Arrange data

``` r
cols_outcome2 <- c("ace_score", "longtermmem", "shorttermmem", "execfunc", "procspeed")
cols_pred2 <- c("all_days_sleeptime", "all_days_sedtime", "all_days_lighttime", "all_days_mvtime")
cols_want <- c(cols_outcome2, cols_pred2)

#create new dataframe 'correlationdat' which contains all rows from final_data but only cols_want columns
correlationdat <- final_dat[, cols_want]
```

Compute correlation matrix using `cor.mat` (Pearson correlations)

``` r
cor.mat <- correlationdat %>% cor_mat()
cor.mat

write.csv(cor.mat, "globalcogcorrelations.csv")
```

Compute p values corresponding to r values using `cor.pmat`

``` r
cor.pmat <- correlationdat %>% cor_pmat()
cor.pmat

write.csv(cor.pmat, "globalcogcorrelations_pvals.csv")
```

#### 5. Compositional data analysis

Arrange data

\*\*\* Note that the order in which the predictor variables have been
arranged will impact the interpretation of the isometric log ratios. In
this instance, we are setting up the ilrs so that the first ilr
represents sleep:remaining behaviours, the second ilr represents
sedentary:active behaviours (light PA + MVPA), and the third ilr
represents light PA:MVPA. All three ilrs are entered in to linear
regression models to capture the entire 24-hour time-use composition.

``` r
cols_pred <- c("all_days_sleeptime","all_days_sedtime","all_days_lighttime", "all_days_mvtime")

head(act_dat)
act_dat <- act_dat[ ,c(cols_outcome, cols_pred, cols_covar)]
head(act_dat)
```

Create sequential binary partition matrix

``` r
sbp4 = matrix(c( 1, -1, -1,-1, 
                 0, 1, -1, -1,
                 0, 0, 1, -1),
              ncol=4, byrow=TRUE)


psi4 = gsi.buildilrBase(t(sbp4)) 
```

Compute ilrs of all rows, but only columns in `cols_pred` (time use
variables)

``` r
ilrs = ilr(acomp(act_dat[, (cols_pred)]), V=psi4) #compute ilrs of all rows but only columns in cols_pred
head(ilrs)

# rename ilrs to ilr1, ilr2 and ilr3
colnames(ilrs) <- paste0("ilr", 1:3)
head(ilrs)
nrow(ilrs)
nrow(act_dat)
```

Create new dataframe ‘`ilr_dat`’ that contains the data to be used in
regression models

``` r
ilr_dat <- cbind(act_dat[, c(cols_outcome, cols_covar)], ilrs)

# triple check there are no NAs in the data
table(is.na(ilr_dat))
```

Set up first series of regression models as follows:

-   Model 1 = intercept only (for data exploration purposes)

``` r
ilr_mod_1 <- lm(ace_score ~ 1, dat = ilr_dat)
```

-   Model 2 = global cognition \~ covariates (age, sex, site, smoking
    status, education) + interaction data (sleep quality, TV watching
    time, recreational PA time)

``` r
ilr_mod_2 <- lm(ace_score ~ visit_age + sex + site_ch + smoke_stat_ch + edu_yrs + sleepqual_ch + tvtime_ch + recpa_ch, dat = ilr_dat)
```

-   Model 3 = global cognition \~ Model 2 + time-use composition

\*\*\*Model 3 investigates the primary research question of the
manuscript (is global cognition associated with 24-hour time-use
composition (after adjusting for covariates)).

\*\*\*`cbind(ilr1, ilr2, ilr3)` represents 24-hour time-use composition.

``` r
ilr_mod_3 <- lm(ace_score ~ visit_age + sex + site_ch + smoke_stat_ch + edu_yrs + sleepqual_ch + tvtime_ch + recpa_ch + cbind(ilr1, ilr2, ilr3), dat = ilr_dat)
```

Summarise linear regression outputs

``` r
summary(ilr_mod_1)
summary(ilr_mod_2)
summary(ilr_mod_3)
```

Set up second series of linear regression models, which each add an
interaction term with respective time-use context measures:

-   Model 4 = global cognition \~ Model 3 + (time-use composition \*
    sleep quality)

``` r
ilr_mod_4 <- lm(ace_score ~ visit_age + sex + site_ch + smoke_stat_ch + edu_yrs + sleepqual_ch + tvtime_ch + recpa_ch + cbind(ilr1, ilr2, ilr3) +
                   (sleepqual_ch * cbind(ilr1, ilr2, ilr3)), dat = ilr_dat)

summary(ilr_mod_4)
```

-   Model 5 = global cognition \~ Model 3 + (time-use composition \* TV
    watching time)

``` r
ilr_mod_5 <- lm(ace_score ~ visit_age + sex + site_ch + smoke_stat_ch + edu_yrs + sleepqual_ch + 
                  tvtime_ch + recpa_ch + cbind(ilr1, ilr2, ilr3) + 
                  tvtime_ch * cbind(ilr1, ilr2, ilr3), dat = ilr_dat)

summary(ilr_mod_5)
```

-   Model 6 = global cognition \~ Model 3 + (time-use composition \*
    recreational PA time)

``` r
ilr_mod_6 <- lm(ace_score ~ visit_age + sex + site_ch + smoke_stat_ch + edu_yrs + sleepqual_ch + tvtime_ch + recpa_ch + cbind(ilr1, ilr2, ilr3) + recpa_ch * cbind(ilr1, ilr2, ilr3), dat = ilr_dat)

summary(ilr_mod_6)
```

Backwards selection of final models (please see Methods: Statistical
Analysis section of manuscript for details on this process)

``` r
car::Anova(ilr_mod_6)
car::Anova(ilr_mod_5)
car::Anova(ilr_mod_4)
car::Anova(ilr_mod_3)
car::Anova(ilr_mod_2)
```

Adjust p-values of final model

-   This requires the p-values from the final ANOVA output to be
    extracted and entered in to a dataframe `pvals`

``` r
pvals <- c(0.4349531, 0.2684554, 0.0066742, 0.5656478, 0.0009408, 0.0593062, 0.1498296, 0.6799961)
          
p.adjust(pvals, method = "BH", n=length(pvals))
```
