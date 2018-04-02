---
output: 
  html_document: 
    keep_md: yes
---
## Commands relating to the app

This will be where the annotation elements of the display will be described:


```r
library(dplyr)
library(mosaic)
```

You can do it this way

```r
t.test(hp ~ mpg > 25, data = mtcars, var.equal = TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  hp by mpg > 25
## t = 3.2196, df = 30, p-value = 0.003079
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   32.03925 143.19151
## sample estimates:
## mean in group FALSE  mean in group TRUE 
##            163.1154             75.5000
```

Or this way, which is more general

```r
mod <- lm(hp ~ cyl, data = mtcars)
summary(mod)
```

```
## 
## Call:
## lm(formula = hp ~ cyl, data = mtcars)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -54.61 -25.99 -11.28  21.51 130.39 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -51.054     24.982  -2.044   0.0499 *  
## cyl           31.958      3.884   8.229 3.48e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.62 on 30 degrees of freedom
## Multiple R-squared:  0.693,	Adjusted R-squared:  0.6827 
## F-statistic: 67.71 on 1 and 30 DF,  p-value: 3.478e-09
```
