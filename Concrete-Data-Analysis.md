Concrete Compressive Strength - Data Analysis
================

### By Martin Bauer

![unnamed-chunk-4-1](https://github.com/martinbauer1/Concrete-Compressive-Strength-Data-Analysis/assets/154390228/001f4ecb-08aa-42ee-815e-03d116b6eb43)
![unnamed-chunk-5-1](https://github.com/martinbauer1/Concrete-Compressive-Strength-Data-Analysis/assets/154390228/cd8f9170-0889-46ca-8bfb-53d2a30d088e)
![unnamed-chunk-6-1](https://github.com/martinbauer1/Concrete-Compressive-Strength-Data-Analysis/assets/154390228/2ecd1fc6-f411-4fdb-b7af-3d723826603b)
![unnamed-chunk-7-1](https://github.com/martinbauer1/Concrete-Compressive-Strength-Data-Analysis/assets/154390228/d8969d63-5354-49ef-93ca-8cd211a0d9e6)
![unnamed-chunk-8-1](https://github.com/martinbauer1/Concrete-Compressive-Strength-Data-Analysis/assets/154390228/a8bc019d-91c5-451b-aba4-0a0acd7659e1)
![unnamed-chunk-9-1](https://github.com/martinbauer1/Concrete-Compressive-Strength-Data-Analysis/assets/154390228/618dddde-e92f-4d71-8098-db45f78f390e)
![unnamed-chunk-10-1](https://github.com/martinbauer1/Concrete-Compressive-Strength-Data-Analysis/assets/154390228/ace924b3-f884-4221-8ca5-d3d48ceec9a5)

## Purpose

The purpose of this project is to better understand why some concrete
might be stronger than others and attempt to estimate the optimal amount
of materials needed using sample data. Some of these materials might
include cement, water, ash, etc., but how much of each ingredient makes
stronger concrete? Concrete is a material frequently used in the
Construction industry, so knowing the right combination of components to
make concrete stronger could prove to be very useful.

Generally, concrete slabs should be at least 2,500 PSI or more in
strength, but some structures may require an even higher PSI. (PSI means
pounds per square inch and is used to measure pressure or “strength” in
this case) For example, sidewalks might require concrete to have a
strength of around 3,000 to 3,500 PSI while a parking garage might
require that of around 4,000 to 4,500 PSI.

<br>

## A. Introduction

### Step 1: Importing the dataset and libraries

The data source is pulled from Kaggle.com and can be found using the
following link:

Data source:
<https://www.kaggle.com/datasets/sinamhd9/concrete-comprehensive-strength>

The “concrete” dataset includes multiple columns including one field
called the strength of concrete measured in MPa (or Megapascals). Some
of the other attributes include the age of the concrete, how much water
is included in the mixture, how much actual cement is mixed in, etc.

The first step is to install any packages needed, load any libraries
used, and load the dataset from the working directory into the R
environment:

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(car)
```

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(class)
library(gmodels)
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
setwd("~/BI/R/Martin's Directory")
concrete <- read.csv("concrete.csv")
```

<br>

### Step 2: Analyzing the sample’s structure

In attempting to understand what the best combination of ingredients
might be for making the strongest concrete, we can use the data to
provide these insights.

The first step in understanding the dataset would be to summarize the
data. For this project, the response variable (or y) will be the
strength of concrete field while the explanatory variables (or xn) would
include all of the other fields: cement, slag, ash, water, etc.

``` r
str(concrete)
```

    ## 'data.frame':    1030 obs. of  9 variables:
    ##  $ cement      : num  141 169 250 266 155 ...
    ##  $ slag        : num  212 42.2 0 114 183.4 ...
    ##  $ ash         : num  0 124.3 95.7 0 0 ...
    ##  $ water       : num  204 158 187 228 193 ...
    ##  $ superplastic: num  0 10.8 5.5 0 9.1 0 0 6.4 0 9 ...
    ##  $ coarseagg   : num  972 1081 957 932 1047 ...
    ##  $ fineagg     : num  748 796 861 670 697 ...
    ##  $ age         : int  28 14 28 28 28 90 7 56 28 28 ...
    ##  $ strength    : num  29.9 23.5 29.2 45.9 18.3 ...

Looking at the structure of the dataset, it shows that there are 1,030
records of data and 9 fields. The data types for all fields are set as
“num” or decimal number except for the age field, which is set as an
“int” or integer.

1.  Cement: mixture of cement measured in kg
2.  Slag: mixture of blast furnace slag measured in kg
3.  Ash: mixture of fly ash measured in kg
4.  Water: water measured in kg
5.  Superplastic: compound of superplasticizer measured in kg
6.  Coarseagg: mixture of coarse aggregates (such as: sand, gravel or
    stone) measured in kg
7.  Fineagg: mixture of fine aggregate measured in kg
8.  Age: measured in days (1 to 365)
9.  Strength: the compressive strength of the concrete measured in MPa

- MPa: Megapascal or a unit of pressure (1 MPa is equal to 145 PSI or
  pounds per square inch)

<br>

### Step 3: Gathering summary statistics

``` r
summary(concrete)
```

    ##      cement           slag            ash             water      
    ##  Min.   :102.0   Min.   :  0.0   Min.   :  0.00   Min.   :121.8  
    ##  1st Qu.:192.4   1st Qu.:  0.0   1st Qu.:  0.00   1st Qu.:164.9  
    ##  Median :272.9   Median : 22.0   Median :  0.00   Median :185.0  
    ##  Mean   :281.2   Mean   : 73.9   Mean   : 54.19   Mean   :181.6  
    ##  3rd Qu.:350.0   3rd Qu.:142.9   3rd Qu.:118.30   3rd Qu.:192.0  
    ##  Max.   :540.0   Max.   :359.4   Max.   :200.10   Max.   :247.0  
    ##   superplastic      coarseagg         fineagg           age        
    ##  Min.   : 0.000   Min.   : 801.0   Min.   :594.0   Min.   :  1.00  
    ##  1st Qu.: 0.000   1st Qu.: 932.0   1st Qu.:731.0   1st Qu.:  7.00  
    ##  Median : 6.400   Median : 968.0   Median :779.5   Median : 28.00  
    ##  Mean   : 6.205   Mean   : 972.9   Mean   :773.6   Mean   : 45.66  
    ##  3rd Qu.:10.200   3rd Qu.:1029.4   3rd Qu.:824.0   3rd Qu.: 56.00  
    ##  Max.   :32.200   Max.   :1145.0   Max.   :992.6   Max.   :365.00  
    ##     strength    
    ##  Min.   : 2.33  
    ##  1st Qu.:23.71  
    ##  Median :34.45  
    ##  Mean   :35.82  
    ##  3rd Qu.:46.13  
    ##  Max.   :82.60

The summary statistics above describe the distributions of each field
measured by the minimum/maximum values as well as mean/median and other
statistics. Fortunately, the dataset does not have any NA’s in any of
the columns, which means less data cleaning. The following histograms
provide visual aids to the statistics above:

<br>

### Histogram of Strength - Positive Skew

``` r
hist(concrete$strength, main = "Distribution of Strength", xlab = "Strength", ylab = "Frequency")
abline(v = median(concrete$strength, na.rm = T), col = "red")
abline(v = mean(concrete$strength, na.rm = T), col = "blue")
legend("topright", c("Mean", "Median"), col=c("blue", "red"), lwd=10)
```

![](Concrete-Data-Analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

- Looking at the strength field, the mean is 35.82 MPa while its median
  is 34.45 MPa. Since the mean is greater than the median, in this case,
  that means that distribution is positively skewed or skews to the
  right. The implication of these statistics is that most of the data
  points for strength are less than the mean. This point is supported by
  the fact that the maximum strength of 82.6 MPa, causing a larger mean.

<br>

### Histogram of Age - Negative Skew

``` r
hist(concrete$age, main = "Distribution of Age", xlab = "Age", ylab = "Frequency")
abline(v = median(concrete$age, na.rm = T), col = "red")
abline(v = mean(concrete$age, na.rm = T), col = "blue")
legend("topright", c("Mean", "Median"), col=c("blue", "red"), lwd=10)
```

![](Concrete-Data-Analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

- The age is another example of a field that has a mean greater than the
  median, but this point is more dramatic considering a mean of 45.66
  days and a median of 28 days. The max age is 365 days (a full year!),
  causing this large positive skew.

<br>

### Histogram of Ash - No Relationship

``` r
hist(concrete$ash, main = "Distribution of Ash", xlab = "Ash", ylab = "Frequency")
abline(v = median(concrete$ash, na.rm = T), col = "red")
abline(v = mean(concrete$ash, na.rm = T), col = "blue")
legend("topright", c("Mean", "Median"), col=c("blue", "red"), lwd=10)
```

![](Concrete-Data-Analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

- Three fields (Slag, Ash, and Superplastic) show 0’s for minimum values
  and 1st quartile values. This indicates that some concrete mixtures do
  not include one or all of these components. It seems to be the case
  that these components are not always required to create concrete, but
  it remains to be known if a lack of any of them cause concrete to be
  stronger or weaker.

<br>

## B. Exploratory Data Analysis

The next step in getting to know the concrete data would be to explore
how variables relate to one another. More specifically, are there any
strong relationships between the response variable (strength) and the
explanatory variables?

The methods of EDA or Exploratory Data Analysis can be executed using
visualizations (such as: scatterplots, boxplots, and bar graphs) as well
as statistical analysis in order to find patterns in the data. One note
to keep in mind is that just because an explanatory variable has a
relationship (positive or negative) with the response variable does not
necessarily prove that more or less of any variable causes concrete to
be stronger in every case.

### Linear Relationships

``` r
plot(x=concrete$cement, y=concrete$strength, main="Concrete Strength by Cement Mixture", xlab="Cement Mixture (kg)", ylab="Concrete Strength (MPa)", type="p")
grid(); points(x=concrete$cement, y=concrete$strength); abline(lm(concrete$strength ~ concrete$cement, data=concrete), col='red')
```

![](Concrete-Data-Analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

The scatterplot above seems to represent a positive relationship between
two variables, which are Strength (y) and Cement (x).

- The red regression line seems to indicate that as more cement mixture
  is added, the concrete’s strength tends to have a higher MPa or become
  stronger. Looking at the scatterplot, it is apparent that this is not
  always the case. The data reveals that in some cases, cement can be
  very high while strength can be very low.

``` r
plot(x=concrete$water, y=concrete$strength, main="Concrete Strength by Water", xlab="Water (kg)", ylab="Concrete Strength (MPa)", type="p")
grid(); points(x=concrete$water, y=concrete$strength); abline(lm(concrete$strength ~ concrete$water, data=concrete), col='blue')
```

![](Concrete-Data-Analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

The second scatterplot shows the opposite relationship between the
response variable, Strength (y), and a different explanatory variable,
Water (x).

- This negative relationship seems to mean that as more water is added,
  the concrete’s strength tends to have a lower MPa or become weaker.
  Again, that is not always case, looking at all the datapoints in the
  visual. While estimating the optimal level of water to be used is not
  yet known, it is worth noting that the minimum level of water used in
  this dataset is 120kg.

``` r
plot(x=concrete$ash, y=concrete$strength, main="Concrete Strength by Ash", xlab="Ash (kg)", ylab="Concrete Strength (MPa)", type="p")
grid(); points(x=concrete$ash, y=concrete$strength); abline(lm(concrete$strength ~ concrete$ash, data=concrete), col='orange')
```

![](Concrete-Data-Analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The third scatterplot shows a slightly different picture than the second
plot.

- Based on the orange regression line, there seems to be a very weak
  negative relationship between the response variable, Strength (y), and
  the explanatory variable, Ash (x). Since there is not a strong
  relationship between the two variables, this can mean that adding more
  ash to the mixture has little to no impact on the concrete’s strength.
  This point is further highlighted by the fact that levels of concrete
  strength have very high variability when the level of Ash is 0.

<br>

### Correlation Plot

While scatterplots are useful in measuring relationships between
variables, they are limited to a few variables. To view the
relationships among all of the variables, a correlation plot paints a
broader picture.

``` r
t1 <- cor(concrete)
corrplot(t1, method='square', order = 'FPC', type = 'lower', diag = FALSE)
```

![](Concrete-Data-Analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Plot Background

The blocked correlation plot above provides a correlation matrix testing
the relationships among all of the variables in the dataset. The
relationship is measured using the correlation coefficient (also known
as the r value), which are displayed at the bottom from -1 to 1. The
blue color (0 to 1) indicates that two variables have a positive
relationship between each other (as one variable increases, the other
variable increases) while the red color (-1 to 0) indicates that two
variables have a negative relationship. Bigger squares with darker
shades of color show stronger relationships between variables.

<br>

### Insights

- Cement and Strength have the strongest positive relationship, which is
  confirmed by the first scatterplot.
- Age and Strength also have a positive relationship, but it is not as
  strong.
- Water and Strength have a negative relationship, which is consistent
  with the second scatterplot.

<br>

### Additional Finding: Correlation between Water and Superplastic

- The most distinct relationship in the visual seems to be Water and
  Superplastic since it shows the strongest relationship in the form of
  a dark red square. Within that same column, 5 rows above it,
  Superplastic and Strength have a positive relationship.

``` r
cor(concrete$water, concrete$superplastic)
```

    ## [1] -0.6575329

- This value of about -0.66 indicates a strong negative relationship
  between Water and Superplastic. A high correlation between two
  explanatory variables: Water and Superplastic suggests that there is
  multicollinearity or that two independent variables may not be truly
  independent from each other.

<br>

## C. Linear Regression

To further explain the relationships between variables, running linear
regression models can provide more insight into whether a relationship
is statistically significant.

A model that is statistically significant means that we can reject the
null hypothesis or status quo with more than 95% confidence. This
ultimately means that the chance of the model’s end-result being random
is less than 5%. The statistic for capturing that chance of randomness
for any given variable is called its p-value.

<br>

### SLR Model - Strength by Cement

The first model will be a simple linear regression (SLR) model that uses
one explanatory variable (cement) to attempt to predict the output of
the response variable. This model uses ordinary least squares method to
measure the correlation of the relationship:

``` r
slr_model <- lm(strength ~ cement, data=concrete)
summary(slr_model)
```

    ## 
    ## Call:
    ## lm(formula = strength ~ cement, data = concrete)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -40.593 -10.952  -0.569   9.990  43.240 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 13.442528   1.296948   10.37   <2e-16 ***
    ## cement       0.079580   0.004324   18.40   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14.5 on 1028 degrees of freedom
    ## Multiple R-squared:  0.2478, Adjusted R-squared:  0.2471 
    ## F-statistic: 338.7 on 1 and 1028 DF,  p-value: < 2.2e-16

Based on this first model, it is confirmed again that strength and
cement have a positive relationship, similar to previous visuals.

- The p-value, or probability of the end-result being random, is very
  low (\<2X10^-16) and less than the significance level of 0.05, so we
  can reasonably claim that cement has a statistically significant
  relationship with strength.

- Given that the explanatory coefficient is ~0.08, we can conclude that
  as cement mixture is increased by +1 kg, the strength of concrete is
  increased by +0.08 MPa.

- Since only one explanatory variable is used in this model, this only
  captures a subset of the data story. This is further supported by the
  low adjusted R-squared amount of 0.2471. The adjusted R-square
  explains the variance of the response variable. This means that this
  model only explains about 25% of the variance in the response
  variable.

<br>

### MLR Model - Strength by All Variables

Given that the first model only explains 25% of the variability in the
response variable, a new model is needed to capture more of the
variance. This Multiple Linear regression (MLR) model includes all
explanatory variables and improves upon the first one.

``` r
mlr_model <- lm(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data=concrete)
summary(mlr_model)
```

    ## 
    ## Call:
    ## lm(formula = strength ~ cement + slag + ash + water + superplastic + 
    ##     coarseagg + fineagg + age, data = concrete)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.654  -6.302   0.703   6.569  34.450 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -23.331214  26.585504  -0.878 0.380372    
    ## cement         0.119804   0.008489  14.113  < 2e-16 ***
    ## slag           0.103866   0.010136  10.247  < 2e-16 ***
    ## ash            0.087934   0.012583   6.988 5.02e-12 ***
    ## water         -0.149918   0.040177  -3.731 0.000201 ***
    ## superplastic   0.292225   0.093424   3.128 0.001810 ** 
    ## coarseagg      0.018086   0.009392   1.926 0.054425 .  
    ## fineagg        0.020190   0.010702   1.887 0.059491 .  
    ## age            0.114222   0.005427  21.046  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.4 on 1021 degrees of freedom
    ## Multiple R-squared:  0.6155, Adjusted R-squared:  0.6125 
    ## F-statistic: 204.3 on 8 and 1021 DF,  p-value: < 2.2e-16

### Insights

- While cement is still statistically significant, its coefficient
  increased from ~0.08 in the first model to ~0.12. We can infer that
  when considering the effects from other components, a bit more cement
  mixture is needed to increase strength.

- Cement, Slag, and Age have the largest effects on Strength given that
  they are the most statistically significant variables.

- Ash, Water, and Superplastic are also statistically significant.

- Water is the only variable that has a negative relationship with
  strength. We can conclude that as you add 1 kg of water to the
  mixture, the strength of concrete decreases by -0.15 MPa.

- The adjusted R-square value improved to 61% as more variance in the
  response variable is reflected in the model.

- Coarse aggregates and Fine aggregates have p-values that are greater
  than the significance level of 0.05, meaning they are technically not
  statistically significant. However, since these values would still
  pass a test of 90% confidence, it is fine to keep these variables in
  the model.

<br>

### Predictions

The models can then be used to make a prediction on what the strength of
concrete might be. What might the strength of concrete be if the means
of each of the explanatory variables are used in the model?

### Prediction 1: SLR Model

``` r
sample1 <- data.frame(cement = 281)
predict(slr_model, newdata=sample1, interval = "prediction")
```

    ##       fit      lwr      upr
    ## 1 35.8046 7.346664 64.26254

<br>

### Prediction 2: MLR Model

``` r
sample2 <- data.frame(cement = 281, slag = 74, ash = 54, water = 182, superplastic = 6, coarseagg = 973, fineagg = 774, age = 46)
predict(mlr_model, newdata=sample2, interval = "prediction")
```

    ##        fit      lwr      upr
    ## 1 35.71596 15.29988 56.13203

<br>

### Insights

- A concrete mixture using the means for each component as their inputs
  is predicted to have a strength of around 35.7 to 35.8 MPa. This range
  is right in line with the mean of the response variable.

- Although both models return similar estimates for strength, Model 2 is
  more reliable considering it has a smaller margin of error within the
  prediction interval (15.3, 56.1).

<br>

## D. Machine Learning

Now that we have explored the data through various techniques, we can
try to create a model that can predict how strong concrete would be
given a select number of fields. The mean strength is about 35.82 MPa,
so that value can serve as the threshold for gauging concrete strength.

- If the predicted value of strength is greater than 35.82 MPa, we can
  say that it is “Higher”.

- If the predicted value of strength is less than 35.82 MPa, we can say
  that it is “Lower”.

<br>

### Step 1: Configure a new dataset

In this step, we will create a new dataset to use as well as configure
the strength column to be binary instead of having a range of values.

``` r
concrete_new <- concrete

set_strength <- function(x) {
  if(x >= 35.82) { 
    strength_bin <- "Higher"} 
  else { strength_bin <- "Lower"}
}

concrete_new$strength_bin <- sapply(concrete_new$strength, FUN=set_strength)
concrete_new$strength_bin <- as.factor(concrete_new$strength_bin)

concrete$strength_bin <- sapply(concrete$strength, FUN=set_strength)
concrete$strength_bin <- as.factor(concrete$strength_bin)

str(concrete_new)
```

    ## 'data.frame':    1030 obs. of  10 variables:
    ##  $ cement      : num  141 169 250 266 155 ...
    ##  $ slag        : num  212 42.2 0 114 183.4 ...
    ##  $ ash         : num  0 124.3 95.7 0 0 ...
    ##  $ water       : num  204 158 187 228 193 ...
    ##  $ superplastic: num  0 10.8 5.5 0 9.1 0 0 6.4 0 9 ...
    ##  $ coarseagg   : num  972 1081 957 932 1047 ...
    ##  $ fineagg     : num  748 796 861 670 697 ...
    ##  $ age         : int  28 14 28 28 28 90 7 56 28 28 ...
    ##  $ strength    : num  29.9 23.5 29.2 45.9 18.3 ...
    ##  $ strength_bin: Factor w/ 2 levels "Higher","Lower": 2 2 2 1 2 2 2 1 2 2 ...

We can delete the old strength column from the new dataset since it is
no longer needed:

``` r
concrete_new <- concrete_new[-9]
concrete <- concrete[-9]
str(concrete_new)
```

    ## 'data.frame':    1030 obs. of  9 variables:
    ##  $ cement      : num  141 169 250 266 155 ...
    ##  $ slag        : num  212 42.2 0 114 183.4 ...
    ##  $ ash         : num  0 124.3 95.7 0 0 ...
    ##  $ water       : num  204 158 187 228 193 ...
    ##  $ superplastic: num  0 10.8 5.5 0 9.1 0 0 6.4 0 9 ...
    ##  $ coarseagg   : num  972 1081 957 932 1047 ...
    ##  $ fineagg     : num  748 796 861 670 697 ...
    ##  $ age         : int  28 14 28 28 28 90 7 56 28 28 ...
    ##  $ strength_bin: Factor w/ 2 levels "Higher","Lower": 2 2 2 1 2 2 2 1 2 2 ...

``` r
str(concrete)
```

    ## 'data.frame':    1030 obs. of  9 variables:
    ##  $ cement      : num  141 169 250 266 155 ...
    ##  $ slag        : num  212 42.2 0 114 183.4 ...
    ##  $ ash         : num  0 124.3 95.7 0 0 ...
    ##  $ water       : num  204 158 187 228 193 ...
    ##  $ superplastic: num  0 10.8 5.5 0 9.1 0 0 6.4 0 9 ...
    ##  $ coarseagg   : num  972 1081 957 932 1047 ...
    ##  $ fineagg     : num  748 796 861 670 697 ...
    ##  $ age         : int  28 14 28 28 28 90 7 56 28 28 ...
    ##  $ strength_bin: Factor w/ 2 levels "Higher","Lower": 2 2 2 1 2 2 2 1 2 2 ...

<br>

### Step 2: Normalize the data

Each component in the concrete dataset has a unique set of statistics
(minimum, maximum, mean, median, etc.), so it can be difficult to
compare these results to each other. That’s why it is important to scale
all columns (except the response variable) down to a range of 0 to 1,
also known as normalization.

``` r
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
```

``` r
concrete_new <- as.data.frame(lapply(concrete_new[1:8], normalize))
summary(concrete_new)
```

    ##      cement            slag              ash             water       
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.2063   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.3442  
    ##  Median :0.3902   Median :0.06121   Median :0.0000   Median :0.5048  
    ##  Mean   :0.4091   Mean   :0.20561   Mean   :0.2708   Mean   :0.4774  
    ##  3rd Qu.:0.5662   3rd Qu.:0.39775   3rd Qu.:0.5912   3rd Qu.:0.5607  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000  
    ##   superplastic      coarseagg         fineagg            age         
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:0.0000   1st Qu.:0.3808   1st Qu.:0.3436   1st Qu.:0.01648  
    ##  Median :0.1988   Median :0.4855   Median :0.4654   Median :0.07418  
    ##  Mean   :0.1927   Mean   :0.4998   Mean   :0.4505   Mean   :0.12270  
    ##  3rd Qu.:0.3168   3rd Qu.:0.6640   3rd Qu.:0.5770   3rd Qu.:0.15110  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000

<br>

### Step 3: Separating the data into training and testing

The training set is used to find the right fit for our model to predict
values while the testing set will be used to evaluate the performance of
our training set.

- 80% of the normalized dataset will be divided into our training set
  while the other 20% will be put in our testing set.

- Target labels will also need to be extracted from the original dataset
  so that the model knows “where to aim at”, so to speak.

``` r
cn_train <- concrete_new[1:800,]
cn_test <- concrete_new[801:1030,]

cn_train_labels <- concrete[1:800, 9]
cn_test_labels <- concrete[801:1030, 9]
```

<br>

### Step 4: Creating and testing the model

For the purposes of this project, the k-nearest neighbors (or knn) model
will be tested to see if it is a good fit in predicting concrete
strength.

#### Hypothesis:

- This model was chosen because the data fields revealed differentiating
  relationships based on summary statistics and regression. These
  findings suggest that the data points of these fields together are
  classified into classes within the data. These classes are formed by
  clusters of multiple components or “nearest neighbors”.

- These clusters can be used to predict whether or not concrete strength
  will be higher or lower than the mean strength of 35.8 MPa.

``` r
cn_test_pred <- knn(train = cn_train, test = cn_test, cl = cn_train_labels)
CrossTable(x = cn_test_labels, y = cn_test_pred, prop.chisq = FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  230 
    ## 
    ##  
    ##                | cn_test_pred 
    ## cn_test_labels |    Higher |     Lower | Row Total | 
    ## ---------------|-----------|-----------|-----------|
    ##         Higher |       101 |        17 |       118 | 
    ##                |     0.856 |     0.144 |     0.513 | 
    ##                |     0.856 |     0.152 |           | 
    ##                |     0.439 |     0.074 |           | 
    ## ---------------|-----------|-----------|-----------|
    ##          Lower |        17 |        95 |       112 | 
    ##                |     0.152 |     0.848 |     0.487 | 
    ##                |     0.144 |     0.848 |           | 
    ##                |     0.074 |     0.413 |           | 
    ## ---------------|-----------|-----------|-----------|
    ##   Column Total |       118 |       112 |       230 | 
    ##                |     0.513 |     0.487 |           | 
    ## ---------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
confusionMatrix(cn_test_pred, cn_test_labels, positive = "Lower")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction Higher Lower
    ##     Higher    101    17
    ##     Lower      17    95
    ##                                           
    ##                Accuracy : 0.8522          
    ##                  95% CI : (0.7996, 0.8954)
    ##     No Information Rate : 0.513           
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.7041          
    ##                                           
    ##  Mcnemar's Test P-Value : 1               
    ##                                           
    ##             Sensitivity : 0.8482          
    ##             Specificity : 0.8559          
    ##          Pos Pred Value : 0.8482          
    ##          Neg Pred Value : 0.8559          
    ##              Prevalence : 0.4870          
    ##          Detection Rate : 0.4130          
    ##    Detection Prevalence : 0.4870          
    ##       Balanced Accuracy : 0.8521          
    ##                                           
    ##        'Positive' Class : Lower           
    ## 

<br>

### Summary

- Based on the table, we can see that there were 101 true negative cases
  where the model correctly predicted that 101 of the 230 results were
  higher than 35.82 MPa.

- There were 95 true positive cases where the model accurately predicted
  that these cases would be lower than the threshold.

- 34 records out of 230 inaccurately predicted that the strength would
  be higher or lower than the threshold.

- As a result, this model provided an accuracy rate of 85%.

<br>

## Thank you!

<br>
