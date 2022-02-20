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
```
