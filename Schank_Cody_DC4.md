Schank\_Cody\_DC4
================

Data Challenge 4
----------------

### Read and prep the data

``` r
redcard_1 = read.csv("./data/redcard_1.csv", stringsAsFactors = F)
redcard_2 = read.csv("./data/redcard_2.csv", stringsAsFactors = F)

# combine dataframes
redcard = rbind(redcard_1, redcard_2)

# remove unnecessary columns
redcard = redcard[,-which(colnames(redcard) %in% c("X","photoID"))]

# take average of two skin tone ratings
# higher skintone value = darker skin
redcard$skintone = rowMeans(redcard[,c('rater1','rater2')])

# get rid of rows where redCards > 1
redcard = redcard[redcard$redCards <= 1,]

# remove rows with missing position data
redcard = redcard[redcard$position != "",]

# impute height and weight
redcard[is.na(redcard$weight),"weight"] = mean(redcard$weight,na.rm=T)
redcard[is.na(redcard$height),"height"] = mean(redcard$height,na.rm=T)

# keep only rows with no NAs 
redcard = redcard[complete.cases(redcard),]

# this might not be necessary, but just making sure
redcard$refNum = as.factor(redcard$refNum)
redcard$position = as.factor(redcard$position)

# scale continuous variables
redcard$skintone = scale(redcard$skintone)
redcard$weight = scale(redcard$weight)
redcard$height = scale(redcard$height)
```

### Run the model

``` r
library(lme4)
```

    ## Loading required package: Matrix

``` r
# not using height and weight, model is unidentifiable with them
m1 = glmer(redCards ~ skintone + (1|refNum) + (1|position), data=redcard, family="binomial")
```

### Summary of model

``` r
summary(m1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: redCards ~ skintone + (1 | refNum) + (1 | position)
    ##    Data: redcard
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  15088.4  15127.0  -7540.2  15080.4   115987 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.3128 -0.1228 -0.0957 -0.0809 15.4553 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  refNum   (Intercept) 0.56728  0.7532  
    ##  position (Intercept) 0.04644  0.2155  
    ## Number of obs: 115991, groups:  refNum, 2906; position, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.91624    0.08302 -59.220   <2e-16 ***
    ## skintone     0.06413    0.02561   2.504   0.0123 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## skintone -0.012
