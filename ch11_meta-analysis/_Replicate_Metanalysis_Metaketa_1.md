Metaketa I - Chapter 11 - Replication
================

Notes on replication script
===========================

-   This .Rmd file produces all tables, and figures reported in Chapter 11 of *Information, Accountability, and Cumulative Learning: Lessons from Metaketa I* in the order in which they appear in the text.

-   The code exports results to folders "tables" and "figures" in this file's directory.

-   Replication options include:

    -   `prep`: if `TRUE`, generate data from raw files, if `FALSE`, loads cleaned data

    -   `n_iter`: number of permutations of treatment assignment used in randomization inference analysis.

    -   `load_iter_matrix`: if `FALSE`, generate `n_iter` columns of treatment assignment according to each study's assignment strategy. If `TRUE`, load existing assignment matrix with 2,000 iterations of treatment assignment.

    -   `ri_action`: treatment effects and *p*-values from randomization inference can be generated from scratch if set to `"generate"`, loaded from existing file if (`"load"`), or not returned (`"ignore"`) in results. Note that these estimations may take a few hours depending on the value of `n_iter`.

    -   `run_bayes`: if `TRUE`, will run Monte Carlo simulations. `run_bayes <- FALSE` will skip the Bayesian analysis.

    -   `load_specification_matrices <- FALSE` will estimate, from scratch, treatment effects under all combinations of specification choices. Likewise, `load_mde_data <- FALSE` will re-run the diagnostic process for the minimal detectable effect analysis. Setting these options to `FALSE` will take several hours to run on an average machine.

    -   `full_m1`: if `FALSE` turns off imputing `0`s for vote choice for cases with where voter turnout is 0 in the cleaned data.

-   For a full clean replication empty the folders `figures` and `tables` and subfolder `data/temp` and set options: `prep <- TRUE`, `ri_action <- "generate"`, `run_bayes <- TRUE`, `load_iter_matrix <- FALSE`, `load_specification_matrices <- FALSE`, `load_mde_data <- FALSE`.

-   This code requires installation of Rstan (unless `run_bayes <- FALSE`). See installation instructions [here](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

-   The package `xlsx` requires a 64 bit Java install on PCs; for more detail see <https://java.com/en/download/win10.jsp>.

``` r
  prep <- TRUE
  n_iter <- 2000
  load_iter_matrix <- TRUE
  ri_action <- "load"
  run_bayes <- TRUE
  load_specification_matrices <- TRUE
  load_mde_data <- TRUE
  full_m1 <- TRUE
```

``` r
if(prep){
  madat <- pool_studies(add_vars= c("nij", "Q_alt", "N_alt",
                                    "lc5.councillor.party.switch", "lc5.chair.party.switch", "lc5.councillor.redistricted2016"))
  if(full_m1){
    madat$m1[is.na(madat$m1) & madat$m3==0] <- 0
    #m3==0 in Burkina Faso means unlikely to vote or definitely not voting.
    madat$m1[madat$m1==1 & madat$m3==0 & madat$ctry == "bf"] <- 0
    madat_raw <- pool_studies(dir = "data/temp")
    madat$m1_against <- ifelse(madat_raw$m1==0 & madat$m3==1, 1, 0)  
    madat$m1_against[is.na(madat$m1_against) & madat$m3==0 & madat$ctry != "bf"] <- 0
    madat$m1_against[madat$m1==1 & madat$m3==0 & madat$ctry == "bf"] <- 0
    madat$m1NA <- (is.na(madat$m1) & madat$m3 == 1)
      
  }
  save(madat, file = "data/temp/madat.Rda")
  write.csv(madat, "madat.csv")
}

load("data/temp/madat.Rda")
 
# Load previously generated permutation matrices
if(load_iter_matrix){
  load("data/temp/assign_iter.Rda")
}else{
  system.time(assign_iter <- assign_i(madat, n_iter))
  save(assign_iter, file = "data/temp/assign_iter.Rda")
}
  madat_iter <- cbind.data.frame(madat, assign_iter)
  rm(assign_iter)
  save(madat_iter, file = "data/temp/madat_iter.Rda")
```

Figure 11.1: Treatment Effect of Information on Incumbent Vote Choice
=====================================================================

``` r
  cov <- "m14i+m17i+m18i+m20i+m21i+m22i+m24i+m26i+m27i"

  t1c1 <-  results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_all_g_unadj.Rda",
                  good = TRUE, depvar = "m1", exclude_councilors = FALSE)
  t1c2 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_ben_g_unadj.Rda",
                  depvar = "m1", weights = FALSE, good = TRUE, country = "ben")
  t1c3 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_brz_g_unadj.Rda",
                  depvar = "m1", weights = TRUE,  good = TRUE, country = "brz")
  t1c4 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_bf_g_unadj.Rda",
                  depvar = "m1", weights = FALSE, good = TRUE, country = "bf")
  t1c5 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_mex_g_unadj.Rda",
                  depvar = "m1", weights = FALSE, good = TRUE, country = "mex")
  t1c6 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_ug1_g_unadj.Rda",
                  depvar = "m1", weights = FALSE, good = TRUE, country = "ug1")
  t1c7 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_ug2_g_unadj.Rda",
                  depvar = "m1", weights = TRUE,  good = TRUE, country = "ug2",
                  exclude_councilors =FALSE)
  
   t2c1 <-  results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_all_b_unadj.Rda",
                  good = FALSE, depvar = "m1", exclude_councilors =FALSE)
  t2c2 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_ben_b_unadj.Rda",
                  depvar = "m1", weights = FALSE, good = FALSE, country = "ben")
  t2c3 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_brz_b_unadj.Rda",
                  depvar = "m1", weights = TRUE,  good = FALSE, country = "brz")
  t2c4 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_bf_b_unadj.Rda",
                  depvar = "m1", weights = FALSE, good = FALSE, country = "bf")
  t2c5 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_mex_b_unadj.Rda",
                  depvar = "m1", weights = FALSE, good = FALSE, country = "mex")
  t2c6 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_ug1_b_unadj.Rda",
                  depvar = "m1", weights = FALSE, good = FALSE, country = "ug1")
  t2c7 <- results(dat = madat_iter, ri_p = ri_action, sims = n_iter,
                  file_rite_obj = "data/temp/ri_m1pool_ug2_b_unadj.Rda",
                  depvar = "m1", weights = TRUE,  good = FALSE, country = "ug2", 
                  exclude_councilors =FALSE)
```

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-4-1.png" alt="Meta-Analysis: Country-Specific Effects on Vote Choice. Estimated change in the proportion of voters who support an incumbent after receiving good news (left panel) or bad news (right panel) about the politician, compared to receiving no information. Weighted unadjusted estimates. Horizontal lines show 95\% confidence intervals for the estimated change. Entries under each estimate show p-values calculated by randomization inference. In all cases, the differences are close to zero and statistically insignificant." width="\linewidth" />
<p class="caption">
Meta-Analysis: Country-Specific Effects on Vote Choice. Estimated change in the proportion of voters who support an incumbent after receiving good news (left panel) or bad news (right panel) about the politician, compared to receiving no information. Weighted unadjusted estimates. Horizontal lines show 95% confidence intervals for the estimated change. Entries under each estimate show p-values calculated by randomization inference. In all cases, the differences are close to zero and statistically insignificant.
</p>

<!-- ![](figures/fig_11.3_incumbent_ctry_unadj.pdf){width=90%} -->

Figure 11.2: Treatment Effect of Information on Voter Turnout
=============================================================

<!-- ![](figures/fig_11.4_turnout_ctry_unadj.pdf) -->
<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-9-1.png" alt="Meta-Analysis: Country-Specific Effects on Turnout. Estimated change in the proportion of voters who turn out to vote after receiving good news (left panel) or bad news (right panel) about the politician, compared to receiving no information. Weighted unadjusted estimates; substantive results are similar for covariate-adjusted country-specific analysis [see Online Appendix]. Horizontal lines show 95\% confidence intervals for the estimated change. Entries under each estimate show p-values calculated by randomization inference." width="\linewidth" />
<p class="caption">
Meta-Analysis: Country-Specific Effects on Turnout. Estimated change in the proportion of voters who turn out to vote after receiving good news (left panel) or bad news (right panel) about the politician, compared to receiving no information. Weighted unadjusted estimates; substantive results are similar for covariate-adjusted country-specific analysis \[see Online Appendix\]. Horizontal lines show 95% confidence intervals for the estimated change. Entries under each estimate show p-values calculated by randomization inference.
</p>

<!-- ## Consistency of Results Across Studies -->
Table 11.1: Effect of Information on Vote Choice and Turnout
============================================================

``` r
# Treatment effects with nij and covariates
cov <- "m14i+m17i+m18i+m20i+m21i+m22i+m24i+m26i+m27i"

# vote choice
# good news
t1ncovc2 <- results(dat = madat_iter, sims = n_iter, ri_p = ri_action,
                    file_rite_obj = "data/temp/ri_m1pool_all_g_cov.Rda",
                    cov = cov, good = TRUE, with_N = TRUE, weights = TRUE, depvar = "m1", exclude_councilors = FALSE)
# bad news
t2ncovc2 <- results(dat = madat_iter, sims = n_iter, ri_p = ri_action,
                    file_rite_obj = "data/temp/ri_m1pool_all_b_cov.Rda",
                    cov = cov, good = FALSE, with_N = TRUE, weights = TRUE, depvar = "m1", exclude_councilors = FALSE)

# turnout
#good news
t3ncovc2 <- results(dat = madat_iter, sims = n_iter, ri_p = ri_action,
                    file_rite_obj = "data/temp/ri_m3pool_all_g_cov.Rda",
                    cov = cov, good = TRUE, with_N = TRUE, weights = TRUE, depvar = "m3", exclude_councilors = FALSE)
# bad news
t4ncovc2 <- results(dat = madat_iter, sims = n_iter, ri_p = ri_action, 
                    file_rite_obj = "data/temp/ri_m3pool_all_b_cov.Rda",
                    cov = cov, good = FALSE, with_N = TRUE, weights = TRUE, depvar = "m3", exclude_councilors = FALSE)

# vote choice overall
t_m1_o <- results(dat = madat_iter, sims = n_iter, ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_all_o_cov.Rda",
                  cov = cov, with_N = TRUE, depvar = "m1", exclude_councilors =FALSE)

# turnout overall
t_m3_o <- results(dat = madat_iter, sims = n_iter, ri_p = ri_action, file_rite_obj = "data/temp/ri_m3pool_all_o_cov.Rda",
                  cov = cov, with_N = TRUE, depvar = "m3", exclude_councilors =FALSE)
```

<table style="text-align:center">
<caption>
<strong>Effect of Information on Vote Choice and Turnout</strong>
</caption>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="6">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="6" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="6">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Good News
</td>
<td>
Bad News
</td>
<td>
Good News
</td>
<td>
Bad News
</td>
<td>
Overall
</td>
<td>
Overall
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
<td>
(5)
</td>
<td>
(6)
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment
</td>
<td>
0.0002
</td>
<td>
-0.003
</td>
<td>
0.002
</td>
<td>
0.018
</td>
<td>
0.003
</td>
<td>
0.017<sup>\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.015)
</td>
<td>
(0.015)
</td>
<td>
(0.013)
</td>
<td>
(0.012)
</td>
<td>
(0.010)
</td>
<td>
(0.008)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
N<sub>ij</sub>
</td>
<td>
-0.015
</td>
<td>
-0.049<sup>\*\*\*</sup>
</td>
<td>
0.002
</td>
<td>
0.011
</td>
<td>
-0.050<sup>\*\*\*</sup>
</td>
<td>
0.010
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.016)
</td>
<td>
(0.015)
</td>
<td>
(0.014)
</td>
<td>
(0.013)
</td>
<td>
(0.012)
</td>
<td>
(0.011)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* N<sub>ij</sub>
</td>
<td>
-0.012
</td>
<td>
-0.001
</td>
<td>
-0.002
</td>
<td>
0.0001
</td>
<td>
-0.002
</td>
<td>
-0.002
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.020)
</td>
<td>
(0.020)
</td>
<td>
(0.019)
</td>
<td>
(0.015)
</td>
<td>
(0.012)
</td>
<td>
(0.011)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
0.355
</td>
<td>
0.398
</td>
<td>
0.843
</td>
<td>
0.835
</td>
<td>
0.368
</td>
<td>
0.837
</td>
</tr>
<tr>
<td style="text-align:left">
RI p-value
</td>
<td>
0.994
</td>
<td>
0.848
</td>
<td>
0.89
</td>
<td>
0.18
</td>
<td>
0.81
</td>
<td>
0.057
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
13,190
</td>
<td>
12,531
</td>
<td>
14,494
</td>
<td>
13,148
</td>
<td>
25,814
</td>
<td>
27,731
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.298
</td>
<td>
0.281
</td>
<td>
0.200
</td>
<td>
0.160
</td>
<td>
0.274
</td>
<td>
0.165
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

<!-- # Secondary Analysis: A Bayesian Approach -->
Figure 11.3: Bayesian Meta-Analysis: Vote Choice
================================================

``` r
# Stan code used in Fig 11.5 and 11.6 to sample from posterior distributions

  bmodel <- "
  data {
    int<lower=0> J; // number of countries 
    real y[J]; // estimated treatment effects
    real<lower=0> sigma[J]; // s.e. of effect estimates 
  }
  parameters {
    real mu; 
    real<lower=0> tau;
    real eta[J];
  }
  transformed parameters {
    real theta[J];
    for (j in 1:J)
      theta[j] = mu + tau * eta[j];
  }
  model {
    target += normal_lpdf(eta | 0, 1);
    target += normal_lpdf(y | theta, sigma);
  }
  "
```

``` r
  # Function to make data from model
  take_results <- function(L) {
    out <- lapply(L, function(i)  coef(summary(i))[1,1:2])
    t(matrix(unlist(out),nrow = 2))
  }

  ### M1: Incumbent vote ###
  good_m1 <- take_results(lapply(list(t1c1, t1c2, t1c3, t1c4, t1c5, t1c6, t1c7), function(i) i$estimates))
  bad_m1 <- take_results(lapply(list(t2c1, t2c2, t2c3, t2c4, t2c5, t2c6, t2c7), function(i) i$estimates))

  ### M3: Turn out###
  good_m3 <- take_results(lapply(list(t3c1, t3c2, t3c3, t3c4, t3c5, t3c6, t3c7), function(i) i$estimates))
  bad_m3 <- take_results(lapply(list(t4c1, t4c2, t4c3, t4c4, t4c5, t4c6, t4c7), function(i) i$estimates))
```

<!-- # ```{r, out.width="\\linewidth", include=TRUE, fig.align="center", fig.cap=c("Bayesian Meta-analysis: Vote Choice. The solid dots and lines show the estimates from the Bayesian model; the top row shows the overall meta-estimate of $\\mu$ and $\\tau$. The white dots show the original frequentist estimates: in many cases shrinkage can be observed, especially in cases that have effects that are more imprecisely estimated."), echo=FALSE} -->
<!-- # bayes_result(my_data = good_m1, main = "Effects of Good News on Vote Choice") -->
<!-- # bayes_result(my_data = bad_m1,  main = "Effects of Bad News on Vote Choice") -->
<!-- # ``` -->
<!-- #  -->
<!-- ![](figures/fig_11.5_posteriors_incumbent.pdf){width=100%} -->

Figure 11.4: Bayesian Meta-Analysis: Turnout
============================================

    ## Warning in readLines(file, warn = TRUE): incomplete final line
    ## found on 'C:\Users\bicalho\Documents\Github\metaketa\ch11_meta-
    ## analysis\3_ma_bayes.stan'

    ## Warning: There were 196 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-14-1.png" alt="Bayesian Meta-analysis: Turnout. The solid dots and lines show the estimates from the Bayesian model; the top row shows the overall meta-estimate of $\mu$ and $\tau$. The white dots show the original frequentist estimates: in many cases shrinkage can be observed, especially in cases that have effects that are more imprecisely estimated." width="\linewidth" />
<p class="caption">
Bayesian Meta-analysis: Turnout. The solid dots and lines show the estimates from the Bayesian model; the top row shows the overall meta-estimate of *μ* and *τ*. The white dots show the original frequentist estimates: in many cases shrinkage can be observed, especially in cases that have effects that are more imprecisely estimated.
</p>

    ## Warning in readLines(file, warn = TRUE): incomplete final line
    ## found on 'C:\Users\bicalho\Documents\Github\metaketa\ch11_meta-
    ## analysis\3_ma_bayes.stan'

    ## Warning: There were 2 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-14-2.png" alt="Bayesian Meta-analysis: Turnout. The solid dots and lines show the estimates from the Bayesian model; the top row shows the overall meta-estimate of $\mu$ and $\tau$. The white dots show the original frequentist estimates: in many cases shrinkage can be observed, especially in cases that have effects that are more imprecisely estimated." width="\linewidth" />
<p class="caption">
Bayesian Meta-analysis: Turnout. The solid dots and lines show the estimates from the Bayesian model; the top row shows the overall meta-estimate of *μ* and *τ*. The white dots show the original frequentist estimates: in many cases shrinkage can be observed, especially in cases that have effects that are more imprecisely estimated.
</p>

<!-- ![](figures/fig_11.6_posteriors_turnout.pdf) -->

<!-- # Robustness of Results -->
<!-- ## Reliability of outcome data -->
<!-- ## The missing India study -->
<!-- ## Country-specific analyses vs. Meta-analysis -->
<!-- # Making Sense of The Null Findings -->
Figures 11.5 and 11.6: Specification Distribution: Treatment effects under different specification choices
==========================================================================================================

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-16-1.png" alt="Specification Distribution: Vote for Incumbent. Distribution of average treatment effects on vote for incumbent for a given specification choice, varying all other choices. Darkened vertical lines show estimates for which $p&lt;0.05$. The dashed vertical line indicates average treatment effect reported in Table 11.1." width="\linewidth" />
<p class="caption">
Specification Distribution: Vote for Incumbent. Distribution of average treatment effects on vote for incumbent for a given specification choice, varying all other choices. Darkened vertical lines show estimates for which *p* &lt; 0.05. The dashed vertical line indicates average treatment effect reported in Table 11.1.
</p>

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-17-1.png" alt="Specification Distribution: Turnout. Distribution of average treatment effects on voter turnout for a given specification choice, varying all other choices. Darkened vertical lines show estimates for which $p&lt;0.05$. The dashed vertical line indicates average treatment effect reported in Table 11.1." width="\linewidth" />
<p class="caption">
Specification Distribution: Turnout. Distribution of average treatment effects on voter turnout for a given specification choice, varying all other choices. Darkened vertical lines show estimates for which *p* &lt; 0.05. The dashed vertical line indicates average treatment effect reported in Table 11.1.
</p>

<!-- ![](figures/fig_spec_curve_m1.pdf)){width=100%} -->
<!-- ![](figures/fig_spec_curve_m3.pdf)){width=100%} -->
<!-- ## Voter Perceptions: Reception of Information and Updating -->
Figure 11.7: Power analysis of minimal detectable effects
=========================================================

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-18-1.png" alt="Minimal detectable effects: Power analysis of minimal detectable effects, computed using Monte Carlo simulation. The horizontal axis varies the conjectured average treatment effect, while the vertical axis shows statistical power: the probability of rejecting the null hypothesis at $\alpha=0.05$." width="\linewidth" />
<p class="caption">
Minimal detectable effects: Power analysis of minimal detectable effects, computed using Monte Carlo simulation. The horizontal axis varies the conjectured average treatment effect, while the vertical axis shows statistical power: the probability of rejecting the null hypothesis at *α* = 0.05.
</p>

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-18-2.png" alt="Minimal detectable effects: Power analysis of minimal detectable effects, computed using Monte Carlo simulation. The horizontal axis varies the conjectured average treatment effect, while the vertical axis shows statistical power: the probability of rejecting the null hypothesis at $\alpha=0.05$." width="\linewidth" />
<p class="caption">
Minimal detectable effects: Power analysis of minimal detectable effects, computed using Monte Carlo simulation. The horizontal axis varies the conjectured average treatment effect, while the vertical axis shows statistical power: the probability of rejecting the null hypothesis at *α* = 0.05.
</p>

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-18-3.png" alt="Minimal detectable effects: Power analysis of minimal detectable effects, computed using Monte Carlo simulation. The horizontal axis varies the conjectured average treatment effect, while the vertical axis shows statistical power: the probability of rejecting the null hypothesis at $\alpha=0.05$." width="\linewidth" />
<p class="caption">
Minimal detectable effects: Power analysis of minimal detectable effects, computed using Monte Carlo simulation. The horizontal axis varies the conjectured average treatment effect, while the vertical axis shows statistical power: the probability of rejecting the null hypothesis at *α* = 0.05.
</p>

<img src="_Replicate_Metanalysis_Metaketa_1_files/figure-markdown_github/unnamed-chunk-18-4.png" alt="Minimal detectable effects: Power analysis of minimal detectable effects, computed using Monte Carlo simulation. The horizontal axis varies the conjectured average treatment effect, while the vertical axis shows statistical power: the probability of rejecting the null hypothesis at $\alpha=0.05$." width="\linewidth" />
<p class="caption">
Minimal detectable effects: Power analysis of minimal detectable effects, computed using Monte Carlo simulation. The horizontal axis varies the conjectured average treatment effect, while the vertical axis shows statistical power: the probability of rejecting the null hypothesis at *α* = 0.05.
</p>

<!-- ![](figures/MDEs_w_cov_2.pdf){width=100%} -->
Table 11.3: Manipulation check (effect of treatment on correct recollection)
============================================================================

``` r
# source("1d_prep_manipulation.R")
madat_m30 <- madat %>% subset(!is.na(correct))

m30mc1_o <- estimates(dat = madat_m30, depvar = "correct", exclude_councilors = TRUE)
m30mc2_o <- estimates(dat = madat_m30, depvar = "correct", weights = FALSE, country = "ben")
m30mc3_o <- estimates(dat = madat_m30, depvar = "correct", country = "brz")
m30mc4_o <- estimates(dat = madat_m30, depvar = "correct", weights = FALSE, country = "mex")
m30mc5_o <- estimates(dat = madat_m30, depvar = "correct", weights = FALSE, country = "ug1")
m30mc6_o <- estimates(dat = madat_m30, depvar = "correct", weights = TRUE, country = "ug2", exclude_councilors = TRUE)
```

<table style="text-align:center">
<caption>
<strong>Manipulation check: Effect of treatment on correct recollection, pooling good and bad news \[unregistered analysis\]</strong>
</caption>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="6">
Correct Recollection
</td>
</tr>
<tr>
<td>
</td>
<td colspan="6" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Overall
</td>
<td>
Benin
</td>
<td>
Brazil
</td>
<td>
Mexico
</td>
<td>
Uganda 1
</td>
<td>
Uganda 2
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
<td>
(5)
</td>
<td>
(6)
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment
</td>
<td>
0.072<sup>\*\*\*</sup>
</td>
<td>
0.050
</td>
<td>
0.038
</td>
<td>
0.149<sup>\*\*\*</sup>
</td>
<td>
0.119<sup>\*\*\*</sup>
</td>
<td>
-0.0001
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.015)
</td>
<td>
(0.059)
</td>
<td>
(0.021)
</td>
<td>
(0.015)
</td>
<td>
(0.035)
</td>
<td>
(0.008)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
16,173
</td>
<td>
897
</td>
<td>
1,677
</td>
<td>
2,089
</td>
<td>
750
</td>
<td>
10,760
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.320
</td>
<td>
0.276
</td>
<td>
0.378
</td>
<td>
0.137
</td>
<td>
0.035
</td>
<td>
0.205
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
</table>
Table 11.4: Manipulation check (difference between posteriors and priors)
=========================================================================

``` r
madat_m30b <- madat %>% subset(!is.na(post.prior.diff))
madat_m30b$post.prior.diff <- abs(madat_m30b$post.prior.diff)

# overall news
m30mc1_g <- estimates(dat = madat_m30b, good = NULL, depvar = "post.prior.diff", exclude_councilors = TRUE)
m30mc2_g <- estimates(dat = madat_m30b, good = NULL, depvar = "post.prior.diff", weights = FALSE, country = "ben")
m30mc3_g <- estimates(dat = madat_m30b, good = NULL, depvar = "post.prior.diff", country = "brz")
# m30mc5_g <- estimates(dat = madat_m30b, good = TRUE, depvar = "post.prior.diff", weights = FALSE, country = "ug1")
m30mc6_g <- estimates(dat = madat_m30b, good = NULL, depvar = "post.prior.diff", weights = TRUE, country = "ug2", exclude_councilors = TRUE)
```

<table style="text-align:center">
<caption>
<strong>Manipulation check: Absolute difference between posterior and prior beliefs for pooled good and bad news \[unregistered analysis\]</strong>
</caption>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
Absolute difference between posterior and prior beliefs
</td>
</tr>
<tr>
<td>
</td>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Overall
</td>
<td>
Benin
</td>
<td>
Brazil
</td>
<td>
Uganda 2
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment
</td>
<td>
0.006
</td>
<td>
0.063
</td>
<td>
-0.003
</td>
<td>
-0.023
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.025)
</td>
<td>
(0.089)
</td>
<td>
(0.022)
</td>
<td>
(0.023)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
12,704
</td>
<td>
389
</td>
<td>
1,677
</td>
<td>
10,638
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.241
</td>
<td>
0.176
</td>
<td>
0.358
</td>
<td>
0.111
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

Table 11.5: Effect of information on perception of importance of politician effort and honesty
==============================================================================================

``` r
# M5 ----
m5gc1 <- results(dat = madat_iter, good = TRUE, depvar = "m5", exclude_councilors = TRUE, sims = n_iter,
                 ri_p = ri_action, file_rite_obj = "data/temp/ri_m5pool_g_unadj.Rda")
## good news

m5bc1 <- results(dat = madat_iter, good = FALSE, depvar = "m5", exclude_councilors = TRUE, sims = n_iter,
                   ri_p = ri_action, file_rite_obj = "data/temp/ri_m5pool_b_unadj.Rda") ## bad news
# M6 ----
m6gc1 <- results(dat = madat_iter, good = TRUE, depvar = "m6", exclude_councilors = TRUE, sims = n_iter,
                   ri_p = ri_action, file_rite_obj = "data/temp/ri_m6pool_g_unadj.Rda")  ## good news

m6bc1 <- results(dat = madat_iter, good = FALSE, depvar = "m6", exclude_councilors = TRUE, sims = n_iter,
                   ri_p = ri_action, file_rite_obj = "data/temp/ri_m6pool_b_unadj.Rda") ## bad news
```

<table style="text-align:center">
<caption>
<strong>Effect of information on perception of importance of politician effort and honesty</strong>
</caption>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Good News
</td>
<td>
Bad News
</td>
<td>
Good News
</td>
<td>
Bad News
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment effect
</td>
<td>
-0.014
</td>
<td>
-0.051
</td>
<td>
-0.053
</td>
<td>
0.099
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.046)
</td>
<td>
(0.051)
</td>
<td>
(0.047)
</td>
<td>
(0.098)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
2.449
</td>
<td>
2.7
</td>
<td>
2.755
</td>
<td>
2.724
</td>
</tr>
<tr>
<td style="text-align:left">
RI p-value
</td>
<td>
0.8
</td>
<td>
0.466
</td>
<td>
0.36
</td>
<td>
0.756
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
7,039
</td>
<td>
5,963
</td>
<td>
7,278
</td>
<td>
6,755
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.253
</td>
<td>
0.294
</td>
<td>
0.300
</td>
<td>
0.231
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

Table 11.6: Effect of information and source credibility on evaluation of politician effort and honesty
=======================================================================================================

``` r
# Effort
m5_m24c1_g <- results(dat = madat_iter, sims = n_iter, 
                      depvar = "m5", interactvar = "m24", good = TRUE, 
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m5pool_m24_all_g_unadj.Rda")  ## good news

m5_m24c1_b <- results(dat = madat_iter, sims = n_iter,
                      depvar = "m5", interactvar = "m24", good = FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m5pool_m24_all_b_unadj.Rda") ## bad news

# Honesty
m6_m24c1_g <- results(dat = madat_iter, sims = n_iter,
                      depvar = "m6", interactvar = "m24", good = TRUE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m6pool_m24_all_g_unadj.Rda")  ## good news

m6_m24c1_b <- results(dat = madat_iter, sims = n_iter, 
                      depvar = "m6", interactvar = "m24", good = TRUE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m6pool_m24_all_b_unadj.Rda")  ## bad news
```

<table style="text-align:center">
<caption>
<strong>Effect of information and source credibility on evaluation of politician effort and honesty \[unregistered analysis\]</strong>
</caption>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Good News
</td>
<td>
Bad News
</td>
<td>
Good News
</td>
<td>
Bad News
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment
</td>
<td>
-0.034
</td>
<td>
-0.088
</td>
<td>
-0.037
</td>
<td>
-0.037
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.079)
</td>
<td>
(0.090)
</td>
<td>
(0.085)
</td>
<td>
(0.085)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Credible Source
</td>
<td>
-0.051
</td>
<td>
-0.010
</td>
<td>
-0.022
</td>
<td>
-0.022
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.079)
</td>
<td>
(0.081)
</td>
<td>
(0.064)
</td>
<td>
(0.064)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Credible Source
</td>
<td>
0.033
</td>
<td>
0.070
</td>
<td>
0.010
</td>
<td>
0.010
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.095)
</td>
<td>
(0.105)
</td>
<td>
(0.093)
</td>
<td>
(0.093)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
2.451
</td>
<td>
2.703
</td>
<td>
2.75
</td>
<td>
2.75
</td>
</tr>
<tr>
<td style="text-align:left">
RI p-values
</td>
<td>
0.725
</td>
<td>
0.516
</td>
<td>
0.72
</td>
<td>
0.72
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
6,436
</td>
<td>
5,406
</td>
<td>
6,483
</td>
<td>
6,483
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.261
</td>
<td>
0.293
</td>
<td>
0.329
</td>
<td>
0.329
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

Table 11.7: Relationship between evaluation of politician effort and honesty with vote choice
=============================================================================================

``` r
madat_m1pool_g <- estimates(dat = madat_iter, weights = FALSE, depvar = "m1", return_df = TRUE, good = TRUE, exclude_councilors =FALSE) # good

  madat_m1pool_g <- madat_m1pool_g %>%
    group_by(ctry) %>%
    mutate(m5 = m5 - mean(m5, na.rm = T)) %>%
    mutate(m6 = m6 - mean(m6, na.rm = T)) %>%
    ungroup()

m1m5c1g <- felm(m1~m5|fe|0|cl, data = madat_m1pool_g) 
m1m6c1g <- felm(m1~m6|fe|0|cl, data = madat_m1pool_g) 

madat_m1pool_b <- estimates(dat = madat_iter, weights = FALSE, depvar = "m1", return_df = TRUE, good = FALSE, exclude_councilors =FALSE) # bad
  
madat_m1pool_b <- madat_m1pool_b %>%
    group_by(ctry) %>%
    mutate(m5 = m5 - mean(m5, na.rm = T)) %>%
    mutate(m6 = m6 - mean(m6, na.rm = T)) %>%
    ungroup()
    
m1m5c1b <- felm(m1~m5|fe|0|cl, data = madat_m1pool_b) 
m1m6c1b <- felm(m1~m6|fe|0|cl, data = madat_m1pool_b)
```

<table style="text-align:center">
<caption>
<strong>Relationship between evaluation of politician effort and honesty with vote choice \[unregistered analysis\]</strong>
</caption>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
Incumbent vote choice
</td>
</tr>
<tr>
<td>
</td>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="2">
Good news
</td>
<td colspan="2">
Bad news
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Effort
</td>
<td>
0.052<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
0.066<sup>\*\*\*</sup>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.006)
</td>
<td>
</td>
<td>
(0.006)
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Dishonesty
</td>
<td>
</td>
<td>
-0.054<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
-0.026<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.005)
</td>
<td>
</td>
<td>
(0.005)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
11,040
</td>
<td>
11,452
</td>
<td>
10,190
</td>
<td>
10,943
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.229
</td>
<td>
0.217
</td>
<td>
0.282
</td>
<td>
0.266
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

<!-- ## Politician Response -->
Table 11.8: Effect of bad news on politician backlash
=====================================================

``` r
m8bc1 <- results(dat = madat_iter, good = FALSE, depvar = "m8",
                 ri_p = ri_action, file_rite_obj = "data/temp/ri_m8pool_all_b_unadj.Rda", sims = n_iter)

m8bc2 <- results(dat = madat_iter, good = FALSE, depvar = "m8", weights = FALSE, country = "ben",
                 ri_p = ri_action, file_rite_obj = "data/temp/ri_m8pool_ben_b_unadj.Rda", sims = n_iter)

m8bc3 <- results(dat = madat_iter, good = FALSE, depvar = "m8", weights = FALSE, country = "mex",
                 ri_p = ri_action, file_rite_obj = "data/temp/ri_m8pool_mex_b_unadj.Rda", sims = n_iter)
```

<table style="text-align:center">
<caption>
<strong>Effect of bad news on politician backlash</strong>
</caption>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="3">
Politician response / backlash
</td>
</tr>
<tr>
<td>
</td>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Overall
</td>
<td>
Benin
</td>
<td>
Mexico
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment effect
</td>
<td>
0.069<sup>\*</sup>
</td>
<td>
0.068
</td>
<td>
0.070<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.028)
</td>
<td>
(0.057)
</td>
<td>
(0.010)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
0.108
</td>
<td>
0.068
</td>
<td>
0.146
</td>
</tr>
<tr>
<td style="text-align:left">
RI p-value
</td>
<td>
0.089
</td>
<td>
0.438
</td>
<td>
0
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
2,052
</td>
<td>
702
</td>
<td>
1,350
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.623
</td>
<td>
0.504
</td>
<td>
0.848
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

<!-- ## Learning from variation -->
Table 11.10: Effect of moderators on incumbent vote choice
==========================================================

``` r
# Coethnicity
## Good news - Mean centred 
madat_m1_coethnic_mc_g <- prepdat_coethnic(madat_m1pool_g, madat_m1pool_g$m1, 0)
m1_m15mc_g1 <- results(dat = madat_m1_coethnic_mc_g, ri_p = ri_action,
                       file_rite_obj = "data/temp/ri_m1pool_m15_g_unadj.Rda",
                       depvar = "m1", interactvar = "m15", sims = n_iter, exclude_councilors = FALSE)

# Bad news - Mean centred
madat_m1_coethnic_mc_b <- prepdat_coethnic(madat_m1pool_b, madat_m1pool_b$m1, 0)
m1_m15mc_b1 <- results(dat = madat_m1_coethnic_mc_b, ri_p = ri_action, 
                       file_rite_obj = "data/temp/ri_m1pool_m15_b_unadj.Rda",
                       depvar = "m1", interactvar = "m15", sims = n_iter, exclude_councilors = FALSE)
```

``` r
# Copartisan
## Good news - dichotomous 
madat_m1_copart_bin_g <- prepdat_copartisan(madat_m1pool_g, madat_m1pool_g$m1, 1)
m1_m19bin_g1 <- results(dat = madat_m1_copart_bin_g, ri_p = ri_action, 
                       file_rite_obj = "data/temp/ri_m1pool_m19_g_unadj.Rda",
                       depvar = "m1", interactvar = "m19", sims = n_iter, exclude_councilors = FALSE)
```

``` r
## Bad news - dichotomous 
madat_m1_copart_bin_b <- prepdat_copartisan(madat_m1pool_b, madat_m1pool_b$m1, 1)
m1_m19bin_b1 <- results(dat = madat_m1_copart_bin_b, ri_p = ri_action, 
                        file_rite_obj = "data/temp/ri_m1pool_m19_b_unadj.Rda",
                       depvar = "m1", interactvar = "m19", sims = n_iter, exclude_councilors = FALSE)
```

``` r
# Clientelism
## Good news - Mean centred 
madat_m1_client_mc_g <- prepdat_client(madat_m1pool_g, madat_m1pool_g$m1, 0)
m1_m22mc_g1 <- results(dat = madat_m1_client_mc_g, ri_p = ri_action, 
                       file_rite_obj = "data/temp/ri_m1pool_m22_g_unadj.Rda",
                       depvar = "m1", interactvar = "m22", sims = n_iter, exclude_councilors = FALSE)
```

``` r
## Bad news - Mean centred 
madat_m1_client_mc_b <- prepdat_client(madat_m1pool_b, madat_m1pool_b$m1, 0)
m1_m22mc_b1 <- results(dat = madat_m1_client_mc_b, ri_p = ri_action, 
                       file_rite_obj = "data/temp/ri_m1pool_m22_b_unadj.Rda",
                       depvar = "m1", interactvar = "m22", sims = n_iter, exclude_councilors = FALSE)
```

<table style="text-align:center">
<caption>
<strong>Effect of moderators on incumbent vote choice</strong>
</caption>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="6">
Incumbent vote choice
</td>
</tr>
<tr>
<td>
</td>
<td colspan="6" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
<td>
(5)
</td>
<td>
(6)
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment
</td>
<td>
0.018
</td>
<td>
0.0004
</td>
<td>
-0.0001
</td>
<td>
0.013
</td>
<td>
0.001
</td>
<td>
0.004
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.015)
</td>
<td>
(0.022)
</td>
<td>
(0.025)
</td>
<td>
(0.021)
</td>
<td>
(0.014)
</td>
<td>
(0.016)
</td>
</tr>
<tr>
<td style="text-align:left">
Coethnicity
</td>
<td>
-0.022
</td>
<td>
0.0003
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.029)
</td>
<td>
(0.041)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Coethnicity
</td>
<td>
0.058
</td>
<td>
-0.042
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.033)
</td>
<td>
(0.049)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Copartisanship
</td>
<td>
</td>
<td>
</td>
<td>
0.216<sup>\*\*\*</sup>
</td>
<td>
0.289<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
(0.032)
</td>
<td>
(0.028)
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Copartisanship
</td>
<td>
</td>
<td>
</td>
<td>
0.001
</td>
<td>
0.004
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
(0.038)
</td>
<td>
(0.036)
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Clientelism
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
-0.041<sup>\*\*\*</sup>
</td>
<td>
-0.044<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.009)
</td>
<td>
(0.011)
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Clientelism
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.013
</td>
<td>
0.006
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.012)
</td>
<td>
(0.015)
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
0.365
</td>
<td>
0.442
</td>
<td>
0.36
</td>
<td>
0.397
</td>
<td>
0.359
</td>
<td>
0.383
</td>
</tr>
<tr>
<td style="text-align:left">
RI p-values
</td>
<td>
0.297
</td>
<td>
0.989
</td>
<td>
0.998
</td>
<td>
0.564
</td>
<td>
0.938
</td>
<td>
0.814
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
11,502
</td>
<td>
10,320
</td>
<td>
11,688
</td>
<td>
10,999
</td>
<td>
13,246
</td>
<td>
12,288
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.268
</td>
<td>
0.230
</td>
<td>
0.276
</td>
<td>
0.289
</td>
<td>
0.279
</td>
<td>
0.259
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

Table 11.11: Effect of information and context heterogenity on incumbent vote choice
====================================================================================

``` r
# M11 - certainty
m1_m11c1_g <- results(dat = madat_iter, sims = n_iter,
                      depvar = "m1", interactvar = "m11", good = TRUE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m11_all_g.Rda")  # good news

m1_m11c1_b <- results(dat = madat_iter, sims = n_iter, 
                      depvar = "m1", interactvar = "m11", good = FALSE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m11_all_b.Rda") # bad news
```

``` r
# M26 - secret ballot
m1_m26c1_g <- results(dat = madat_iter, sims = n_iter, 
                      depvar = "m1", interactvar = "m26", good = TRUE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m26_all_g.Rda")  # good news
```

``` r
m1_m26c1_b <- results(dat = madat_iter, sims = n_iter, 
                      depvar = "m1", interactvar = "m26", good = FALSE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m26_all_b.Rda") # bad news
```

``` r
# M27 - free and fair election
m1_m27c1_g <- results(dat = madat_iter, sims = n_iter, 
                      depvar = "m1", interactvar = "m27", good = TRUE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m27_all_g.Rda")  # good news
```

``` r
m1_m27c1_b <- results(dat = madat_iter, sims = n_iter, 
                      depvar = "m1", interactvar = "m27", good = FALSE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m27_all_b.Rda") # bad news
```

<table style="text-align:center">
<caption>
<strong>Effect of information and context heterogenity on incumbent vote choice</strong>
</caption>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="6">
Incumbent vote choice
</td>
</tr>
<tr>
<td>
</td>
<td colspan="6" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
<td>
(5)
</td>
<td>
(6)
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment
</td>
<td>
-0.062
</td>
<td>
-0.011
</td>
<td>
0.015
</td>
<td>
-0.005
</td>
<td>
-0.030
</td>
<td>
0.019
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.055)
</td>
<td>
(0.054)
</td>
<td>
(0.024)
</td>
<td>
(0.030)
</td>
<td>
(0.034)
</td>
<td>
(0.034)
</td>
</tr>
<tr>
<td style="text-align:left">
Certainty
</td>
<td>
-0.015
</td>
<td>
0.021
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.017)
</td>
<td>
(0.018)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Certainty
</td>
<td>
0.032
</td>
<td>
-0.003
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.024)
</td>
<td>
(0.024)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Secret ballot
</td>
<td>
</td>
<td>
</td>
<td>
-0.001
</td>
<td>
0.010
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
(0.008)
</td>
<td>
(0.010)
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Secret ballot
</td>
<td>
</td>
<td>
</td>
<td>
-0.005
</td>
<td>
0.005
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
(0.010)
</td>
<td>
(0.011)
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Free, fair election
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
-0.001
</td>
<td>
0.005
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.008)
</td>
<td>
(0.009)
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Free, fair election
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.012
</td>
<td>
-0.004
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.010)
</td>
<td>
(0.011)
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control means
</td>
<td>
0.362
</td>
<td>
0.412
</td>
<td>
0.383
</td>
<td>
0.357
</td>
<td>
0.352
</td>
<td>
0.386
</td>
</tr>
<tr>
<td style="text-align:left">
RI p-values
</td>
<td>
0.281
</td>
<td>
0.858
</td>
<td>
0.572
</td>
<td>
0.874
</td>
<td>
0.423
</td>
<td>
0.588
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
10,993
</td>
<td>
9,622
</td>
<td>
13,419
</td>
<td>
12,589
</td>
<td>
13,111
</td>
<td>
12,422
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.328
</td>
<td>
0.267
</td>
<td>
0.258
</td>
<td>
0.235
</td>
<td>
0.262
</td>
<td>
0.239
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

Table 11.12: Effect of information and electoral competition on vote choice
===========================================================================

``` r
# low competition ----
# good news
madat_m1_m25lpool_g <- madat_m1pool_g %>%
    drop_na(m25) %>%
    filter(!ctry %in% c("bf","ug2")) %>%
    group_by(ctry) %>%
    filter(m25 <= median(m25)) %>%
    ungroup()

m1_m25lc1_g <- results(dat = madat_m1_m25lpool_g, sims = n_iter,
                       ri_p = ri_action, file_rite_obj = "data/temp/r1_m1pool_m25l_g.Rda",
                       good = TRUE, depvar = "m1")
rm(madat_m1_m25lpool_g)
```

``` r
# bad news 
madat_m1_m25lpool_b <- madat_m1pool_b %>%
    drop_na(m25) %>%
    filter(!ctry %in% c("bf","ug2")) %>%
    group_by(ctry) %>%
    filter(m25 <= median(m25)) %>%
    ungroup()

m1_m25lc1_b <- results(dat = madat_m1_m25lpool_b, sims = n_iter,
                       ri_p = ri_action, file_rite_obj = "data/temp/r1_m1pool_m25l_b.Rda",
                       good = FALSE, depvar = "m1")
rm(madat_m1_m25lpool_b)
```

``` r
# high competition ----
# good news
madat_m1_m25hpool_g <- madat_m1pool_g %>%
    drop_na(m25) %>%
    filter(!ctry %in% c("bf","ug2")) %>%
    group_by(ctry) %>%
    filter(m25 > median(m25)) %>%
    ungroup()

m1_m25hc1_g <- results(dat = madat_m1_m25hpool_g, sims = n_iter,
                       ri_p = ri_action, file_rite_obj = "data/temp/r1_m1pool_m25h_g.Rda",
                       good = TRUE, depvar = "m1", skip_prep = TRUE)
rm(madat_m1_m25hpool_g)
```

``` r
# bad news regressions
madat_m1_m25hpool_b <- madat_m1pool_b %>%
    drop_na(m25) %>%
    filter(ctry!="bf"&ctry!="ug2") %>%
    group_by(ctry) %>%
    filter(m25 > median(m25)) %>%
    ungroup()


m1_m25hc1_b <- results(dat = madat_m1_m25hpool_b, sims = n_iter,
                       ri_p = ri_action, file_rite_obj = "data/temp/r1_m1pool_m25h_b.Rda",
                       good = FALSE, depvar = "m1", skip_prep = TRUE)
rm(madat_m1_m25hpool_b)
```

<table style="text-align:center">
<caption>
<strong>Effect of information and electoral competition on vote choice</strong>
</caption>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
Incumbent vote choice
</td>
</tr>
<tr>
<td>
</td>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment
</td>
<td>
0.009
</td>
<td>
-0.043
</td>
<td>
0.004
</td>
<td>
0.015
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.022)
</td>
<td>
(0.031)
</td>
<td>
(0.030)
</td>
<td>
(0.037)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
0.342
</td>
<td>
0.414
</td>
<td>
0.392
</td>
<td>
0.294
</td>
</tr>
<tr>
<td style="text-align:left">
RI p-values
</td>
<td>
0.716
</td>
<td>
0.254
</td>
<td>
0.904
</td>
<td>
0.746
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
1,450
</td>
<td>
1,433
</td>
<td>
1,113
</td>
<td>
1,307
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.221
</td>
<td>
0.231
</td>
<td>
0.240
</td>
<td>
0.128
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

Table 11.13: Effect of information and intervention-specific heterogenity on vote choice
========================================================================================

``` r
t1nc1_g <- results(dat = madat_iter, sims = n_iter, depvar = "m1", good = TRUE, with_N = TRUE, exclude_councilors =FALSE,
                   ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_N_all_g.Rda")   #good news, nij

t1nc1_b <- results(dat = madat_iter, sims = n_iter, depvar = "m1", good = FALSE, with_N = TRUE, exclude_councilors =FALSE,
                   ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_N_all_b.Rda")  #bad news, nij
```

``` r
# M24 - credibliity
m1_m24c1_g <- results(dat = madat_iter, sims = n_iter, depvar = "m1", interactvar = "m24", good = TRUE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m24_all_g.Rda") #good news

m1_m24c1_b <- results(dat = madat_iter, sims = n_iter, depvar = "m1", interactvar = "m24", good = FALSE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m24_all_b.Rda") #bad news
```

``` r
# m23 - salience
m1_m23c1_g <- results(dat = madat_iter, sims = n_iter, depvar = "m1", interactvar = "m23", good = TRUE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m23_all_g.Rda")  #good news

m1_m23c1_b <- results(dat = madat_iter, sims = n_iter, depvar = "m1", interactvar = "m23", good = FALSE, exclude_councilors =FALSE,
                      ri_p = ri_action, file_rite_obj = "data/temp/ri_m1pool_m23_all_b.Rda") #bad news
```

<table style="text-align:center">
<caption>
<strong>Effect of information and intervention-specific heterogenity on vote choice</strong>
</caption>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="6">
Incumbent vote choice
</td>
</tr>
<tr>
<td>
</td>
<td colspan="6" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
<td>
Good news
</td>
<td>
Bad news
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
<td>
(5)
</td>
<td>
(6)
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment
</td>
<td>
0.001
</td>
<td>
-0.010
</td>
<td>
0.025
</td>
<td>
-0.022
</td>
<td>
-0.017
</td>
<td>
-0.013
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.016)
</td>
<td>
(0.016)
</td>
<td>
(0.024)
</td>
<td>
(0.036)
</td>
<td>
(0.021)
</td>
<td>
(0.023)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
N<sub>ij</sub>
</td>
<td>
-0.027
</td>
<td>
-0.053<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.016)
</td>
<td>
(0.014)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* N<sub>ij</sub>
</td>
<td>
-0.006
</td>
<td>
-0.006
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.020)
</td>
<td>
(0.019)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Information salient
</td>
<td>
</td>
<td>
</td>
<td>
-0.016
</td>
<td>
-0.041
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
(0.029)
</td>
<td>
(0.035)
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Information salient
</td>
<td>
</td>
<td>
</td>
<td>
-0.015
</td>
<td>
0.053
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
(0.034)
</td>
<td>
(0.042)
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Credible source
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
-0.007
</td>
<td>
0.005
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.028)
</td>
<td>
(0.027)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Treatment \* Credible source
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.036
</td>
<td>
0.020
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.030)
</td>
<td>
(0.031)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
0.356
</td>
<td>
0.398
</td>
<td>
0.355
</td>
<td>
0.435
</td>
<td>
0.363
</td>
<td>
0.385
</td>
</tr>
<tr>
<td style="text-align:left">
RI p-values
</td>
<td>
0.956
</td>
<td>
0.592
</td>
<td>
0.314
</td>
<td>
0.61
</td>
<td>
0.452
</td>
<td>
0.628
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
13,274
</td>
<td>
12,563
</td>
<td>
12,343
</td>
<td>
10,587
</td>
<td>
12,354
</td>
<td>
11,407
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.275
</td>
<td>
0.249
</td>
<td>
0.265
</td>
<td>
0.221
</td>
<td>
0.260
</td>
<td>
0.240
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

<!-- # Looking Forward: Does Public Information Boost
Informational Effects? -->
Table 11.14: Private vs public information: effect of good news on incumbent vote choice
========================================================================================

``` r
# Load and append data
files <- list.files("data/temp/pubpvt", pattern = "pp_covinter", full.names = TRUE)
for (f in files) {
  load(f)
}

madat <- rbind.fill(ben, mexsur, ug1)
madat$inv_wts <- NA
madat$inv_wts2 <- NA

m1c1_g <- results(ri_p = "ignore", depvar = "m1", treat1and2 = TRUE, good = TRUE)
m1c2_g <- results(ri_p = "ignore", depvar = "m1", treat1and2 = TRUE, good = TRUE, 
                  country = "ben", weights = FALSE)
m1c3_g <- results(ri_p = "ignore", depvar = "m1", treat1and2 = TRUE, good = TRUE, 
                  country = "mex", weights = FALSE)
m1c4_g <- results(ri_p = "ignore", depvar = "m1", treat1and2 = TRUE, good = TRUE, 
                  country = "ug1", weights = FALSE)
f_g <- f_equalcoeff(estimates(depvar = "m1", treat1and2 = TRUE, good = TRUE, return_df = TRUE), "m1")

m1c1_b <- results(ri_p = "ignore", depvar = "m1", treat1and2 = TRUE, good = FALSE)
m1c2_b <- results(ri_p = "ignore", depvar = "m1", treat1and2 = TRUE, good = FALSE, 
                  country = "ben", weights = FALSE)
m1c3_b <- results(ri_p = "ignore", depvar = "m1", treat1and2 = TRUE, good = FALSE, 
                  country = "mex", weights = FALSE)
m1c4_b <- results(ri_p = "ignore", depvar = "m1", treat1and2 = TRUE, good = FALSE, 
                  country = "ug1", weights = FALSE)
f_b <- f_equalcoeff(estimates(depvar = "m1", treat1and2 = TRUE, good = TRUE, return_df = TRUE), "m1")
```

<table style="text-align:center">
<caption>
<strong>Private vs Public Information: Effect of good news on incumbent vote choice</strong>
</caption>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
Incumbent vote choice, good news
</td>
</tr>
<tr>
<td>
</td>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Overall
</td>
<td>
Benin
</td>
<td>
Mexico
</td>
<td>
Uganda 1
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Private information
</td>
<td>
-0.008
</td>
<td>
0.012
</td>
<td>
-0.029
</td>
<td>
0.008
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.023)
</td>
<td>
(0.044)
</td>
<td>
(0.043)
</td>
<td>
(0.027)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Public information
</td>
<td>
0.055<sup>\*</sup>
</td>
<td>
0.146<sup>\*\*</sup>
</td>
<td>
-0.002
</td>
<td>
0.019
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.022)
</td>
<td>
(0.047)
</td>
<td>
(0.041)
</td>
<td>
(0.023)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
0.356
</td>
<td>
0.439
</td>
<td>
0.498
</td>
<td>
0.186
</td>
</tr>
<tr>
<td style="text-align:left">
F-test p-value
</td>
<td>
0.018
</td>
<td>
0.006
</td>
<td>
0.598
</td>
<td>
0.708
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
2,962
</td>
<td>
776
</td>
<td>
784
</td>
<td>
1,402
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.192
</td>
<td>
0.189
</td>
<td>
0.088
</td>
<td>
0.068
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

Table 11.15: Private vs Public Information: Effect of bad news on incumbent vote choice
=======================================================================================

<table style="text-align:center">
<caption>
<strong>Private vs Public Information: Effect of bad news on incumbent vote choice</strong>
</caption>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
Incumbent vote choice, bad news
</td>
</tr>
<tr>
<td>
</td>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Overall
</td>
<td>
Benin
</td>
<td>
Mexico
</td>
<td>
Uganda 1
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Private information
</td>
<td>
-0.027
</td>
<td>
-0.012
</td>
<td>
-0.036
</td>
<td>
-0.035
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.030)
</td>
<td>
(0.074)
</td>
<td>
(0.030)
</td>
<td>
(0.042)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Public information
</td>
<td>
0.009
</td>
<td>
0.006
</td>
<td>
0.015
</td>
<td>
0.009
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.026)
</td>
<td>
(0.069)
</td>
<td>
(0.032)
</td>
<td>
(0.032)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Control mean
</td>
<td>
0.441
</td>
<td>
0.535
</td>
<td>
0.383
</td>
<td>
0.426
</td>
</tr>
<tr>
<td style="text-align:left">
F-test p-value
</td>
<td>
0.018
</td>
<td>
0.006
</td>
<td>
0.598
</td>
<td>
0.708
</td>
</tr>
<tr>
<td style="text-align:left">
Covariates
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
2,909
</td>
<td>
601
</td>
<td>
1,309
</td>
<td>
999
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.178
</td>
<td>
0.241
</td>
<td>
0.102
</td>
<td>
0.153
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
</table>
