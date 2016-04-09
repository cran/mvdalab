<!-- README.md is generated from README.Rmd. Please edit that file -->
**NOTE: This R package focuses on the implementation of latent variable methods and multivariate modeling tools. The focus is on exploratory analyses using dimensionality reduction methods and classical multivariate statistical tools.**

### Installation

``` r
devtools::install_github("mvdalab/mvdalab")
```

### Quick demo

Fitting a PLS model:

``` r
library(mvdalab)
data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], 
               ncomp = 3, contr = "contr.niets", method = "bidiagpls", 
               validation = "oob")

summary(mod1)
#> Call:
#> 
#> plsFit(formula = log.RAI ~ ., ncomp = 3, data = Penta[, -1], 
#>     contr = "contr.niets", method = "bidiagpls", scale = TRUE, 
#>     validation = "oob")
#> 
#> Coefficients:
#>        Estimate Bootstrap Error   't value'         bias 'bias t value'
#> L3  0.438131130      0.07813105  5.60764388 -0.056287765    -0.72042756
#> S3 -0.339839340      0.08368706 -4.06083517  0.044477379     0.53147262
#> P1 -0.210181974      0.06071820 -3.46159762  0.057955814     0.95450482
#> S1 -0.135884870      0.06997010 -1.94204192  0.019907955     0.28452089
#> P3  0.111249534      0.06854336  1.62305337  0.034862926     0.50862586
#> S2  0.089752422      0.04701730  1.90892350  0.006892843     0.14660228
#> L2  0.071367951      0.04526160  1.57678793 -0.019359275    -0.42771960
#> L4  0.069951677      0.07232330  0.96720806 -0.004522244    -0.06252818
#> L5  0.035696148      0.04552429  0.78411217  0.008591990     0.18873419
#> P4 -0.028238597      0.05462905 -0.51691543 -0.020000349    -0.36611199
#> P2 -0.025167765      0.06283351 -0.40054683 -0.012729431    -0.20258984
#> S4  0.020226747      0.06658432  0.30377644 -0.037592328    -0.56458231
#> L1  0.017465764      0.06489465  0.26914025 -0.006988723    -0.10769335
#> S5  0.010701880      0.04456740  0.24012801 -0.004024037    -0.09029106
#> P5 -0.002811084      0.04681625 -0.06004504  0.003525576     0.07530667
#> 
#> Fit Summary: 
#> 
#> Number of objects = 30
#> Number of predictor variables = 15
#> Method: bidiagpls
#> Design Matrix for Factors = contr.niets
#> No. of bootstrap samples =  1000
#> Number of components considered
#> in above parameter estimates = 3
#> R2X = 0.228 0.389 0.485
#> R2Y = 0.691 0.824 0.874
#> Out-of-Bag R2 (per component) = 0.446 0.458 0.354
#> Out-of-Bag PRESS (per component) = 4.335 3.902 4.455
#> Out-of-Bag MSPRESS.632 (per component) = 0.335 0.263 0.286
#> Out-of-Bag RMSPRESS.632 (per component) = 0.578 0.512 0.535
```

PCA via NIPALS.

``` r
library(mvdalab)
my.nipals <- pca.nipals(iris[, 1:4], ncomps = 4, tol = 1e-08)
names(my.nipals)
#> [1] "Loadings"      "Scores"        "Loading.Space" "Score.Space"

my.nipals$Loadings
#>                     [,1]        [,2]        [,3]       [,4]
#> Sepal.Length  0.36138514  0.65659919 -0.58203416  0.3154592
#> Sepal.Width  -0.08452411  0.73015136  0.59793829 -0.3196944
#> Petal.Length  0.85667099 -0.17337204  0.07625627 -0.4798353
#> Petal.Width   0.35828937 -0.07548926  0.54579393  0.7536837
svd(scale(iris[, 1:4], scale = FALSE))$v
#>             [,1]        [,2]        [,3]       [,4]
#> [1,]  0.36138659 -0.65658877  0.58202985  0.3154872
#> [2,] -0.08452251 -0.73016143 -0.59791083 -0.3197231
#> [3,]  0.85667061  0.17337266 -0.07623608 -0.4798390
#> [4,]  0.35828920  0.07548102 -0.54583143  0.7536574
```

Traditional Multivariate Mean Vector Comparison.

``` r
library(mvdalab)
data(College)
dat1 <- College
#Generate a 'fake' difference of 15 units
dat2 <- College + matrix(rnorm(nrow(dat1) * ncol(dat1), mean = 15), 
        nrow = nrow(dat1), ncol = ncol(dat1))

Comparison <- MVComp(dat1, dat2, level = .95)
Comparison
#>   lower 95 % confidence upper 95 % confidence    Significance
#> 1             -47.66009              17.95686 Not Significant
#> 2             -19.69886             -10.07013     Significant
#> 3             -16.88621             -12.75947     Significant
```
