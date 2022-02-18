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
# calculate CI - choice of confint vs confint.default??
round(confint(logit_fit),3)
```

    ## Waiting for profiling to be done...

    ##              2.5 % 97.5 %
    ## (Intercept) -3.206 -1.557
    ## dose         0.830  1.546

``` r
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
# make prediction ??
round(predict.glm(logit_fit, newdata = data.frame(rep(0.01, 5)),type = 'response'), 3) # type = "response" gives the predicted probabilities
```

    ##     1     2     3     4     5 
    ## 0.089 0.238 0.500 0.762 0.911

``` r
round(predict.glm(logit_fit,newdata=data.frame(x=0.01),type='r'),4)
```

    ## Warning: 'newdata' had 1 row but variables found have 5 rows

    ##      1      2      3      4      5 
    ## 0.0892 0.2383 0.5000 0.7617 0.9108

## Probit link

## Log-log link
