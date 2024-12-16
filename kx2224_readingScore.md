Reading_score
================
Kangyu Xu (kx2224)
2024-12-08

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(broom)
```

## Data Preparation

``` r
df = read_csv("data/Project_1_data.csv")
```

    ## Rows: 948 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): Gender, EthnicGroup, ParentEduc, LunchType, TestPrep, ParentMarita...
    ## dbl  (4): NrSiblings, MathScore, ReadingScore, WritingScore
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
score_df = df %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.character), as.factor))

score_df_sib <- score_df %>%
  mutate(nr_siblings = as.factor(nr_siblings))

# Check for missing values
colSums(is.na(score_df))
```

    ##                gender          ethnic_group           parent_educ 
    ##                     0                    59                    53 
    ##            lunch_type             test_prep parent_marital_status 
    ##                     0                    55                    49 
    ##        practice_sport        is_first_child           nr_siblings 
    ##                    16                    30                    46 
    ##       transport_means      wkly_study_hours            math_score 
    ##                   102                    37                     0 
    ##         reading_score         writing_score 
    ##                     0                     0

``` r
score_df_omit = score_df|> 
  na.omit()

score_df_mean = score_df|>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) |>
   na.omit()

score_df_sib_mean = score_df_sib|>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) |>
   na.omit()
```

``` r
score_df_mean
```

    ## # A tibble: 618 × 14
    ##    gender ethnic_group parent_educ    lunch_type test_prep parent_marital_status
    ##    <fct>  <fct>        <fct>          <fct>      <fct>     <fct>                
    ##  1 female group B      master's degr… standard   none      single               
    ##  2 male   group C      some college   standard   none      married              
    ##  3 female group B      associate's d… standard   none      married              
    ##  4 female group B      some college   standard   completed widowed              
    ##  5 male   group B      some college   free/redu… none      married              
    ##  6 male   group D      high school    free/redu… completed single               
    ##  7 female group B      high school    free/redu… none      married              
    ##  8 male   group D      associate's d… standard   none      divorced             
    ##  9 female group B      high school    standard   none      married              
    ## 10 male   group A      some college   standard   completed single               
    ## # ℹ 608 more rows
    ## # ℹ 8 more variables: practice_sport <fct>, is_first_child <fct>,
    ## #   nr_siblings <dbl>, transport_means <fct>, wkly_study_hours <fct>,
    ## #   math_score <dbl>, reading_score <dbl>, writing_score <dbl>

## Build model

``` r
model_without<- lm(reading_score ~ gender + ethnic_group + parent_educ +
                     lunch_type + test_prep + parent_marital_status + 
                     practice_sport + is_first_child + nr_siblings + 
                     transport_means + wkly_study_hours,
                   data = score_df_omit)
model_with =  lm(reading_score ~ gender + ethnic_group + parent_educ +
                     lunch_type + test_prep + parent_marital_status + 
                     practice_sport + is_first_child + nr_siblings + 
                     transport_means + wkly_study_hours + math_score +
                     writing_score,
                   data = score_df_omit)
```

``` r
summary(model_with)
```

    ## 
    ## Call:
    ## lm(formula = reading_score ~ gender + ethnic_group + parent_educ + 
    ##     lunch_type + test_prep + parent_marital_status + practice_sport + 
    ##     is_first_child + nr_siblings + transport_means + wkly_study_hours + 
    ##     math_score + writing_score, data = score_df_omit)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.7449  -2.7447   0.1746   2.7233   9.9356 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   3.69895    1.45510   2.542 0.011287 *  
    ## gendermale                   -0.19934    0.55991  -0.356 0.721958    
    ## ethnic_groupgroup B          -0.20764    0.68127  -0.305 0.760650    
    ## ethnic_groupgroup C          -0.83449    0.65112  -1.282 0.200506    
    ## ethnic_groupgroup D          -2.25779    0.65963  -3.423 0.000665 ***
    ## ethnic_groupgroup E          -0.71730    0.74051  -0.969 0.333133    
    ## parent_educbachelor's degree -0.35888    0.59700  -0.601 0.547995    
    ## parent_educhigh school        0.82897    0.52110   1.591 0.112215    
    ## parent_educmaster's degree   -1.19104    0.75046  -1.587 0.113057    
    ## parent_educsome college      -0.61008    0.51662  -1.181 0.238138    
    ## parent_educsome high school   1.16579    0.52848   2.206 0.027791 *  
    ## lunch_typestandard           -1.28803    0.38023  -3.388 0.000755 ***
    ## test_prepnone                 1.95426    0.38906   5.023 6.84e-07 ***
    ## parent_marital_statusmarried -0.01477    0.48051  -0.031 0.975494    
    ## parent_marital_statussingle  -0.21686    0.54324  -0.399 0.689895    
    ## parent_marital_statuswidowed -1.38238    1.12474  -1.229 0.219561    
    ## practice_sportregularly      -2.29321    0.56029  -4.093 4.88e-05 ***
    ## practice_sportsometimes      -0.97531    0.54249  -1.798 0.072741 .  
    ## is_first_childyes             0.25206    0.35670   0.707 0.480078    
    ## nr_siblings                  -0.18001    0.11341  -1.587 0.113006    
    ## transport_meansschool_bus     0.12742    0.34247   0.372 0.709998    
    ## wkly_study_hours> 10         -0.31571    0.51904  -0.608 0.543255    
    ## wkly_study_hours10-May       -0.20061    0.39727  -0.505 0.613772    
    ## math_score                    0.12304    0.03052   4.031 6.32e-05 ***
    ## writing_score                 0.87377    0.03264  26.770  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.975 on 562 degrees of freedom
    ## Multiple R-squared:  0.9341, Adjusted R-squared:  0.9313 
    ## F-statistic:   332 on 24 and 562 DF,  p-value: < 2.2e-16

``` r
summary(model_without)
```

    ## 
    ## Call:
    ## lm(formula = reading_score ~ gender + ethnic_group + parent_educ + 
    ##     lunch_type + test_prep + parent_marital_status + practice_sport + 
    ##     is_first_child + nr_siblings + transport_means + wkly_study_hours, 
    ##     data = score_df_omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -41.754  -8.793   0.635   9.118  30.513 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   65.5976     3.6847  17.803  < 2e-16 ***
    ## gendermale                    -7.6725     1.1114  -6.904 1.37e-11 ***
    ## ethnic_groupgroup B           -1.4287     2.2582  -0.633  0.52722    
    ## ethnic_groupgroup C           -0.8558     2.1619  -0.396  0.69236    
    ## ethnic_groupgroup D            2.5663     2.1753   1.180  0.23860    
    ## ethnic_groupgroup E            5.9165     2.3850   2.481  0.01340 *  
    ## parent_educbachelor's degree   2.5549     1.9735   1.295  0.19600    
    ## parent_educhigh school        -5.3732     1.7046  -3.152  0.00171 ** 
    ## parent_educmaster's degree     3.9202     2.4535   1.598  0.11065    
    ## parent_educsome college       -2.3866     1.7136  -1.393  0.16424    
    ## parent_educsome high school   -4.7948     1.7305  -2.771  0.00578 ** 
    ## lunch_typestandard             8.4374     1.1489   7.344 7.31e-13 ***
    ## test_prepnone                 -6.2822     1.1720  -5.360 1.21e-07 ***
    ## parent_marital_statusmarried   5.2439     1.5783   3.322  0.00095 ***
    ## parent_marital_statussingle    1.9235     1.8013   1.068  0.28605    
    ## parent_marital_statuswidowed   5.5863     3.7208   1.501  0.13381    
    ## practice_sportregularly       -0.6843     1.8590  -0.368  0.71292    
    ## practice_sportsometimes        0.6757     1.7998   0.375  0.70749    
    ## is_first_childyes              1.3046     1.1835   1.102  0.27078    
    ## nr_siblings                    0.3882     0.3752   1.035  0.30131    
    ## transport_meansschool_bus      0.2841     1.1351   0.250  0.80247    
    ## wkly_study_hours> 10           1.0970     1.7121   0.641  0.52197    
    ## wkly_study_hours10-May         2.6835     1.3108   2.047  0.04110 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.2 on 564 degrees of freedom
    ## Multiple R-squared:  0.2709, Adjusted R-squared:  0.2425 
    ## F-statistic: 9.527 on 22 and 564 DF,  p-value: < 2.2e-16

## Analyse variables

``` r
significant_vars <- summary(model_without)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  filter(`Pr(>|t|)` < 0.05) %>%
  arrange(`Pr(>|t|)`)

print("显著变量：")
```

    ## [1] "显著变量："

``` r
print(significant_vars)
```

    ##                       Variable  Estimate Std. Error   t value     Pr(>|t|)
    ## 1                  (Intercept) 65.597596   3.684706 17.802669 1.359178e-56
    ## 2           lunch_typestandard  8.437387   1.148940  7.343628 7.311989e-13
    ## 3                   gendermale -7.672487   1.111353 -6.903736 1.368298e-11
    ## 4                test_prepnone -6.282193   1.172025 -5.360118 1.214232e-07
    ## 5 parent_marital_statusmarried  5.243931   1.578327  3.322461 9.500656e-04
    ## 6       parent_educhigh school -5.373215   1.704583 -3.152218 1.706419e-03
    ## 7  parent_educsome high school -4.794754   1.730451 -2.770811 5.776098e-03
    ## 8          ethnic_groupgroup E  5.916469   2.384968  2.480733 1.340177e-02
    ## 9       wkly_study_hours10-May  2.683485   1.310825  2.047173 4.110388e-02

``` r
interaction_model <- step(lm(reading_score ~ gender + ethnic_group + parent_educ + lunch_type +
                               test_prep + practice_sport + wkly_study_hours +
                               gender:wkly_study_hours + test_prep:wkly_study_hours +
                               parent_educ:lunch_type, 
                             data = score_df_omit))
```

    ## Start:  AIC=3068.1
    ## reading_score ~ gender + ethnic_group + parent_educ + lunch_type + 
    ##     test_prep + practice_sport + wkly_study_hours + gender:wkly_study_hours + 
    ##     test_prep:wkly_study_hours + parent_educ:lunch_type
    ## 
    ##                              Df Sum of Sq    RSS    AIC
    ## - parent_educ:lunch_type      5    599.44 100625 3061.6
    ## - test_prep:wkly_study_hours  2    194.59 100220 3065.2
    ## - practice_sport              2    223.78 100249 3065.4
    ## - gender:wkly_study_hours     2    319.98 100346 3066.0
    ## <none>                                    100026 3068.1
    ## - ethnic_group                4   3062.07 103088 3077.8
    ## 
    ## Step:  AIC=3061.61
    ## reading_score ~ gender + ethnic_group + parent_educ + lunch_type + 
    ##     test_prep + practice_sport + wkly_study_hours + gender:wkly_study_hours + 
    ##     test_prep:wkly_study_hours
    ## 
    ##                              Df Sum of Sq    RSS    AIC
    ## - test_prep:wkly_study_hours  2     173.7 100799 3058.6
    ## - practice_sport              2     259.3 100884 3059.1
    ## - gender:wkly_study_hours     2     308.2 100933 3059.4
    ## <none>                                    100625 3061.6
    ## - ethnic_group                4    3172.7 103798 3071.8
    ## - parent_educ                 5    5565.6 106191 3083.2
    ## - lunch_type                  1    9351.8 109977 3111.8
    ## 
    ## Step:  AIC=3058.62
    ## reading_score ~ gender + ethnic_group + parent_educ + lunch_type + 
    ##     test_prep + practice_sport + wkly_study_hours + gender:wkly_study_hours
    ## 
    ##                           Df Sum of Sq    RSS    AIC
    ## - practice_sport           2     274.0 101073 3056.2
    ## - gender:wkly_study_hours  2     300.0 101099 3056.4
    ## <none>                                 100799 3058.6
    ## - ethnic_group             4    3139.3 103938 3068.6
    ## - parent_educ              5    5627.6 106426 3080.5
    ## - test_prep                1    5045.6 105844 3085.3
    ## - lunch_type               1    9496.4 110295 3109.5
    ## 
    ## Step:  AIC=3056.21
    ## reading_score ~ gender + ethnic_group + parent_educ + lunch_type + 
    ##     test_prep + wkly_study_hours + gender:wkly_study_hours
    ## 
    ##                           Df Sum of Sq    RSS    AIC
    ## - gender:wkly_study_hours  2     294.5 101367 3053.9
    ## <none>                                 101073 3056.2
    ## - ethnic_group             4    3038.9 104112 3065.6
    ## - parent_educ              5    5756.9 106830 3078.7
    ## - test_prep                1    5116.5 106189 3083.2
    ## - lunch_type               1    9533.9 110607 3107.1
    ## 
    ## Step:  AIC=3053.92
    ## reading_score ~ gender + ethnic_group + parent_educ + lunch_type + 
    ##     test_prep + wkly_study_hours
    ## 
    ##                    Df Sum of Sq    RSS    AIC
    ## <none>                          101367 3053.9
    ## - wkly_study_hours  2     785.9 102153 3054.4
    ## - ethnic_group      4    3082.0 104449 3063.5
    ## - parent_educ       5    5696.8 107064 3076.0
    ## - test_prep         1    5128.2 106496 3080.9
    ## - gender            1    8179.7 109547 3097.5
    ## - lunch_type        1    9613.4 110981 3105.1

``` r
summary(interaction_model)
```

    ## 
    ## Call:
    ## lm(formula = reading_score ~ gender + ethnic_group + parent_educ + 
    ##     lunch_type + test_prep + wkly_study_hours, data = score_df_omit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -43.604  -8.496   0.606   9.826  28.291 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   70.7700     2.7864  25.398  < 2e-16 ***
    ## gendermale                    -7.5943     1.1178  -6.794 2.75e-11 ***
    ## ethnic_groupgroup B           -0.9577     2.2642  -0.423  0.67247    
    ## ethnic_groupgroup C           -0.1638     2.1687  -0.076  0.93982    
    ## ethnic_groupgroup D            2.5066     2.1826   1.148  0.25126    
    ## ethnic_groupgroup E            5.9376     2.3986   2.475  0.01360 *  
    ## parent_educbachelor's degree   2.8661     1.9841   1.445  0.14914    
    ## parent_educhigh school        -5.2694     1.7069  -3.087  0.00212 ** 
    ## parent_educmaster's degree     3.7378     2.4551   1.522  0.12845    
    ## parent_educsome college       -2.2999     1.7186  -1.338  0.18135    
    ## parent_educsome high school   -5.0602     1.7396  -2.909  0.00377 ** 
    ## lunch_typestandard             8.5235     1.1573   7.365 6.20e-13 ***
    ## test_prepnone                 -6.2982     1.1708  -5.379 1.09e-07 ***
    ## wkly_study_hours> 10           1.3557     1.7224   0.787  0.43155    
    ## wkly_study_hours10-May         2.7275     1.3157   2.073  0.03861 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.31 on 572 degrees of freedom
    ## Multiple R-squared:  0.248,  Adjusted R-squared:  0.2296 
    ## F-statistic: 13.47 on 14 and 572 DF,  p-value: < 2.2e-16
