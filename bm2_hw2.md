Homework 2 for P8131
================
Roxy Zhang
2/18/2022

# Question 1

## Format data

``` r
dose = c(0:4)
death = c(2, 8, 15, 23, 27)
survival = rep(30, 5) - death

df1 = data.frame(dose, death, survival)
y1 = as.matrix(df1[ , -1])
```

## Logit link

``` r
# fit a glm model
logit_fit <- glm(
  y1 ~ dose, 
  data = df1,
  family = binomial(link = 'logit'))

summary(logit_fit)
```

    ## 
    ## Call:
    ## glm(formula = y1 ~ dose, family = binomial(link = "logit"), data = df1)
    ## 
    ## Deviance Residuals: 
    ##       1        2        3        4        5  
    ## -0.4510   0.3597   0.0000   0.0643  -0.2045  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -2.3238     0.4179  -5.561 2.69e-08 ***
    ## dose          1.1619     0.1814   6.405 1.51e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 64.76327  on 4  degrees of freedom
    ## Residual deviance:  0.37875  on 3  degrees of freedom
    ## AIC: 20.854
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# extract estimated beta
logit_fit$coefficients
```

    ## (Intercept)        dose 
    ##   -2.323790    1.161895

``` r
round(logit_fit$coefficients[2], 3)
```

    ##  dose 
    ## 1.162

``` r
# calculate CI (asymptotic)
round(confint.default(logit_fit),3)
```

    ##              2.5 % 97.5 %
    ## (Intercept) -3.143 -1.505
    ## dose         0.806  1.517

``` r
# extract deviance
round(logit_fit$deviance, 3)
```

    ## [1] 0.379

``` r
# calculate deviance "by hand"
round(sum(residuals(logit_fit, type = 'deviance' ) ^ 2), 3)
```

    ## [1] 0.379

``` r
# make prediction
round(predict.glm(logit_fit, newdata = data.frame(dose = 0.01), type = 'response'), 4) # type = "response" gives the predicted probabilities
```

    ##      1 
    ## 0.0901

## Probit link

``` r
# fit a glm model
probit_fit <- glm(
  y1 ~ dose, 
  data = df1,
  family = binomial(link = 'probit'))

summary(probit_fit)
```

    ## 
    ## Call:
    ## glm(formula = y1 ~ dose, family = binomial(link = "probit"), 
    ##     data = df1)
    ## 
    ## Deviance Residuals: 
    ##        1         2         3         4         5  
    ## -0.35863   0.27493   0.01893   0.18230  -0.27545  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.37709    0.22781  -6.045 1.49e-09 ***
    ## dose         0.68638    0.09677   7.093 1.31e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 64.76327  on 4  degrees of freedom
    ## Residual deviance:  0.31367  on 3  degrees of freedom
    ## AIC: 20.789
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# extract estimated beta
probit_fit$coefficients
```

    ## (Intercept)        dose 
    ##  -1.3770923   0.6863805

``` r
round(probit_fit$coefficients[2], 3)
```

    ##  dose 
    ## 0.686

``` r
# calculate CI
round(confint.default(probit_fit),3)
```

    ##              2.5 % 97.5 %
    ## (Intercept) -1.824 -0.931
    ## dose         0.497  0.876

``` r
# extract deviance
round(probit_fit$deviance, 3)
```

    ## [1] 0.314

``` r
# calculate deviance "by hand"
round(sum(residuals(probit_fit, type = 'deviance' ) ^ 2), 3)
```

    ## [1] 0.314

``` r
# make prediction
round(predict.glm(probit_fit, newdata = data.frame(dose = 0.01),type = 'response'), 4)
```

    ##      1 
    ## 0.0853

## Log-log link

``` r
# fit a glm model
ll_fit <- glm(
  y1 ~ dose, 
  data = df1,
  family = binomial(link = 'cloglog'))

summary(ll_fit)
```

    ## 
    ## Call:
    ## glm(formula = y1 ~ dose, family = binomial(link = "cloglog"), 
    ##     data = df1)
    ## 
    ## Deviance Residuals: 
    ##       1        2        3        4        5  
    ## -1.0831   0.2132   0.4985   0.5588  -0.6716  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -1.9942     0.3126  -6.378 1.79e-10 ***
    ## dose          0.7468     0.1094   6.824 8.86e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 64.7633  on 4  degrees of freedom
    ## Residual deviance:  2.2305  on 3  degrees of freedom
    ## AIC: 22.706
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# extract estimated beta
ll_fit$coefficients
```

    ## (Intercept)        dose 
    ##  -1.9941520   0.7468193

``` r
round(ll_fit$coefficients[2], 3)
```

    ##  dose 
    ## 0.747

``` r
# calculate CI
round(confint.default(ll_fit),3)
```

    ##              2.5 % 97.5 %
    ## (Intercept) -2.607 -1.381
    ## dose         0.532  0.961

``` r
# extract deviance
round(ll_fit$deviance, 3)
```

    ## [1] 2.23

``` r
# calculate deviance "by hand"
round(sum(residuals(ll_fit, type = 'deviance' ) ^ 2), 3)
```

    ## [1] 2.23

``` r
# make prediction
round(predict.glm(ll_fit, newdata = data.frame(dose = 0.01),type = 'response'), 4)
```

    ##      1 
    ## 0.1282

## Estimate LD50 with 90% CI

``` r
# logit

beta0_logit = logit_fit$coefficients[1]
beta1_logit = logit_fit$coefficients[2]

# inverse fisher information
beta_cov_logit = vcov(logit_fit)

xhat_logit = - beta0_logit/ beta1_logit

# point estimate of x0
round(exp(xhat_logit),3)
```

    ## (Intercept) 
    ##       7.389

``` r
var_xhat_logit = beta_cov_logit[1,1]/(beta1_logit^2) + beta_cov_logit[2,2]*(beta0_logit^2)/(beta1_logit^4)
 - 2*beta_cov_logit[1,2]*beta0_logit/(beta1_logit^3)
```

    ## (Intercept) 
    ##  -0.1950322

``` r
xhat_logit + c(0, qnorm(0.05), - qnorm(0.05)) * sqrt(var_xhat_logit)
```

    ## [1] 2.000000 1.216539 2.783461

``` r
logit_row = round(exp(xhat_logit + c(0, qnorm(0.05), - qnorm(0.05)) * sqrt(var_xhat_logit)), 3)
```

``` r
# probit
beta0_probit = probit_fit$coefficients[1]
beta1_probit = probit_fit$coefficients[2]

beta_cov_probit = vcov(probit_fit)

xhat_probit = - beta0_probit/ beta1_probit

var_xhat_probit = beta_cov_probit[1,1]/(beta1_probit^2) + beta_cov_probit[2,2]*(beta0_probit^2)/(beta1_probit^4)
 - 2*beta_cov_probit[1,2]*beta0_probit/(beta1_probit^3)
```

    ## (Intercept) 
    ##  -0.1597875

``` r
xhat_probit + c(0, qnorm(0.05), - qnorm(0.05)) * sqrt(var_xhat_probit)
```

    ## [1] 2.006310 1.289034 2.723586

``` r
probit_row = round(exp(xhat_probit + c(0, qnorm(0.05), - qnorm(0.05)) * sqrt(var_xhat_probit)), 3)
```

``` r
# log-log
c = log(-log(0.5))

beta0_ll = ll_fit$coefficients[1]
beta1_ll = ll_fit$coefficients[2]

beta_cov_ll = vcov(ll_fit)

xhat_ll = (c - beta0_ll)/ beta1_ll

var_xhat_ll = beta_cov_ll[1,1]/(beta1_ll^2) + beta_cov_ll[2,2]*(c - beta0_ll^2)/(beta1_ll^4) - 2*beta_cov_ll[1,2]*(c - beta0_ll)/(beta1_ll^3)

xhat_ll + c(0, qnorm(0.05), - qnorm(0.05)) * sqrt(var_xhat_ll)
```

    ## [1] 2.179428 1.355016 3.003841

``` r
ll_row = round(exp(xhat_ll + c(0, qnorm(0.05), - qnorm(0.05)) * sqrt(var_xhat_ll)), 3)
```

``` r
ci_df = data.frame(rbind(logit_row, probit_row, ll_row)) %>% 
  rename(
    "Estimate LD50" = X1,
    "90% CI Lower" = X2,
    "90% CI Upper" = X3
  )

row.names(ci_df) = c("logit", "probit", "log-log")

ci_df %>% 
  knitr::kable()
```

|         | Estimate LD50 | 90% CI Lower | 90% CI Upper |
|:--------|--------------:|-------------:|-------------:|
| logit   |         7.389 |        3.375 |       16.175 |
| probit  |         7.436 |        3.629 |       15.235 |
| log-log |         8.841 |        3.877 |       20.163 |

# Question 2

``` r
# input data
amount = seq(10, 90, 5)
offers = c(4, 6, 10, 12, 39, 36, 22, 14, 10, 12, 8, 9, 3, 1, 5, 2, 1)
enrolls = c(0, 2, 4, 2, 12, 14, 10, 7, 5, 5, 3, 5, 2, 0, 4, 2, 1)

df2 = data.frame(amount, offers, enrolls)
```

``` r
# fit a logistic regression model
offer_fit <- glm(
  cbind(enrolls, offers - enrolls) ~ amount, 
  data = df2,
  family = binomial(link = 'logit'))

# conduct Hosmer-Lemeshow test for sparse data
library(ResourceSelection)
```

    ## ResourceSelection 0.3-5   2019-07-22

``` r
hoslem.test(offer_fit$y, fitted(offer_fit), g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  offer_fit$y, fitted(offer_fit)
    ## X-squared = 1.6111, df = 8, p-value = 0.9907

``` r
# model interpretation
summary(offer_fit)
```

    ## 
    ## Call:
    ## glm(formula = cbind(enrolls, offers - enrolls) ~ amount, family = binomial(link = "logit"), 
    ##     data = df2)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4735  -0.6731   0.1583   0.5285   1.1275  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.64764    0.42144  -3.910 9.25e-05 ***
    ## amount       0.03095    0.00968   3.197  0.00139 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 21.617  on 16  degrees of freedom
    ## Residual deviance: 10.613  on 15  degrees of freedom
    ## AIC: 51.078
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# standard error
sqrt(vcov(offer_fit)[2,2])
```

    ## [1] 0.009679756

``` r
# CI for odds ratio
exp(confint.default(offer_fit))
```

    ##                  2.5 %    97.5 %
    ## (Intercept) 0.08427711 0.4397136
    ## amount      1.01205048 1.0511895

``` r
# scholarship of 40% enrollment rate
x0fit = (log(2/3) + 1.648) / 0.031

# 95% CI
beta0_offer = offer_fit$coefficients[1]
beta1_offer = offer_fit$coefficients[2]
betacov = vcov(offer_fit)

varx0 = betacov[1,1]/(beta1_offer^2)+betacov[2,2]*((c-beta0_offer)^2)/(beta1_offer^4)
+2*betacov[1,2]*(c-beta0_offer)/(beta1_offer^3)
```

    ## (Intercept) 
    ##   -329.2271

``` r
round((x0fit+c(qnorm(0.025),0,-qnorm(0.025))*sqrt(varx0)),3)
```

    ## [1]  3.257 40.082 76.906
