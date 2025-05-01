Lab 12 - Smoking during pregnancy
================
Allison Li
20250430

### Load packages and data

``` r
##install.packages("openintro")
##install.packages("infer")
library(infer)
library(tidyverse) 
library(tidymodels)
library(openintro)

##set a seed
set.seed(1234)
```

### Exercise 1

``` r
data(ncbirths)

boxplot(ncbirths$mage, main = "of Mother's Age", ylab = "Age")
```

![](lab-12_files/figure-gfm/outliers-1.png)<!-- -->

``` r
boxplot(ncbirths$fage, main = "Father's Age", ylab = "Age")
```

![](lab-12_files/figure-gfm/outliers-2.png)<!-- -->

``` r
boxplot(ncbirths$weeks, main = "length of pregnancy", ylab = "Weeks")
```

![](lab-12_files/figure-gfm/outliers-3.png)<!-- -->

``` r
boxplot(ncbirths$visits, main = "number of hospital visits during pregnancy", ylab = "Times")
```

![](lab-12_files/figure-gfm/outliers-4.png)<!-- -->

``` r
boxplot(ncbirths$gained, main = "weight gained by mother during pregnancy", ylab = "Pounds")
```

![](lab-12_files/figure-gfm/outliers-5.png)<!-- -->

``` r
boxplot(ncbirths$weight, main = "weight of the baby at birth", ylab = "Pounds")
```

![](lab-12_files/figure-gfm/outliers-6.png)<!-- -->

Categorical variables:mature, premie, marital, lowbirthweight, gender,
habit, whitemom Numeric variables:fage, mage, weeks, visits, gained,
weight There is one outlier for mage, two for fage, multiple (11) for
weeks, 7 for visits, multiple for gained, and many outliers for weight.

The cases are the comprehensive birth record for infants in North
Carolina, 2004. Each case in the dataset is the information about their
father and mother during pregancy for each infant. There are 1000 cases
in the sample

### Exercise 2

``` r
ncbirths_white <- ncbirths %>%
  filter(whitemom == "white")

mean(ncbirths_white$weight, na.rm = TRUE)
```

    ## [1] 7.250462

### Exercise 3

I believe the criteria are satisfied. The dataset has relatively large
sample (1000) and the observations are random and independent of each
other.

### Exercise 4

``` r
bootstrap <- ncbirths_white %>%
  specify(response = weight) %>%
  generate(reps = 150, type = "bootstrap") %>%
  calculate(stat = "mean")
summary(bootstrap)
```

    ##    replicate           stat      
    ##  Min.   :  1.00   Min.   :7.114  
    ##  1st Qu.: 38.25   1st Qu.:7.210  
    ##  Median : 75.50   Median :7.248  
    ##  Mean   : 75.50   Mean   :7.244  
    ##  3rd Qu.:112.75   3rd Qu.:7.279  
    ##  Max.   :150.00   Max.   :7.380

``` r
obs_mean <- mean(ncbirths_white$weight)
centered_dist <- bootstrap %>%
  mutate(stat = stat - obs_mean + 7.43)

##I asked gpt bc I do not know how to calculate the p value in this way
p_value <- centered_dist %>%
  summarise(
    p_val = mean(abs(stat - 7.43) >= abs(obs_mean - 7.43))
    ) %>%
  pull(p_val)

##Plot
ggplot(data = centered_dist, mapping = aes(x = stat)) +
  geom_histogram(binwidth = .01, fill = "skyblue", color = "pink") +
  labs(title = "Bootstrap distribution of means")
```

![](lab-12_files/figure-gfm/bootstrap-1.png)<!-- -->

The null hypothesis: H0: u = 7.43 Alternative hypothesis: H1: u ≠ 7.43
