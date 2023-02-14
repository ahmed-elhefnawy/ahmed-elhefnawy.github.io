---
title: "Multinomial Logistic Regression"
excerpt: "Quick application of MNL regression using `cars` dataset"
collection: portfolio
---


``` r
#read and assign the data. I made sure that the data file is in the same working directory as the working directory in R.
library(data.table)
dt_cars <- fread(file = "cars.csv", sep = ";")

#create a variable for the Drive type.
dt_cars[, "Drive" := "FWD"][AWD==1, "Drive" := "AWD"][RWD==1, "Drive" := "RWD"][, "Drive" := as.factor(Drive)]
```

$\bullet$ **MNL regression:**

``` r
library(nnet)

#make "RWD" the reference class. The "relevel" will reorder the Drive variable by the reference, RWD.
dt_cars$type<-relevel(dt_cars$Drive,ref="RWD") 

#run multinomial logistic regression, using the new variable created before.
model<-multinom(type~HP,data=dt_cars)
```

    ## # weights:  9 (4 variable)
    ## initial  value 468.008835 
    ## iter  10 value 378.849870
    ## final  value 378.849859 
    ## converged

``` r
library(stargazer)
```

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer

``` r
#visualize the results.
stargazer(model,type="text",digits = 4) #"digits" here displays four decimal places.
```

    ## 
    ## ==============================================
    ##                       Dependent variable:     
    ##                   ----------------------------
    ##                        AWD            FWD     
    ##                        (1)            (2)     
    ## ----------------------------------------------
    ## HP                  -0.0054***    -0.0194***  
    ##                      (0.0021)      (0.0023)   
    ##                                               
    ## Constant             1.1774**      4.9953***  
    ##                      (0.5261)      (0.5279)   
    ##                                               
    ## ----------------------------------------------
    ## Akaike Inf. Crit.    765.6997      765.6997   
    ## ==============================================
    ## Note:              *p<0.1; **p<0.05; ***p<0.01

The coefficients are interpreted relative to the reference. That is, for
AWD, as horse power increases by one unit, the coefficient for AWD
relative to RWD will decrease by 0.005, which means if the horse power
increases by one, the chances of having RWD vehicle are higher compared
to AWD.  

The results are significant at the 1% level.

------------------------------------------------------------------------

$\bullet$ Using the model before:

------------------------------------------------------------------------

$\bullet$ **Predicting AWD and FWD class probabilities, and calculating
the RWD class probability:**[^1]

The multinomial logistic regression model is a log-linear model, such
that:  

and:

The denominator is the reference we specified before. That is, the
coefficients are interpreted relative to the reference, RWD.  

Then, if we exponentiate each side, we have:

In `R`, we can exponentiate the intercept and the coefficients:

``` r
exp(coef(model))
```

    ##     (Intercept)        HP
    ## AWD     3.24581 0.9946333
    ## FWD   147.71311 0.9807867

As equations @ref(eq:5) and @ref(eq:6) have the same denominator, we can
write:

Since the class probabilities have to sum up to 1, that means:

Using equation @ref(eq:8), equation @ref(eq:7) can be re-written as:

Then if we add 1 to both sides, we have:

  

Substituting equation @ref(eq:11) into equations @ref(eq:5) and
@ref(eq:6), we have:

  

In `R`, we can predict the probabilities for each observation:

``` r
prediction<-predict(model, dt_cars,type="prob") #type here is for probability.
head(prediction)
```

    ##         RWD       AWD       FWD
    ## 1 0.2600668 0.2515229 0.4884103
    ## 2 0.2600668 0.2515229 0.4884103
    ## 3 0.3781879 0.2949303 0.3268818
    ## 4 0.4517007 0.3079192 0.2403800
    ## 5 0.1939238 0.2145603 0.5915159
    ## 6 0.3931465 0.2984564 0.3083971

The probabilities must sum to 1. (e.g. each row has to sum up to 1).

For logistic regression, the fitted values represent the probabilities,
not the expectations, so, alternatively, we can use the fitted values of
the model to get the same probabilities for all observations.

``` r
head(fitted(model))
```

    ##         RWD       AWD       FWD
    ## 1 0.2600668 0.2515229 0.4884103
    ## 2 0.2600668 0.2515229 0.4884103
    ## 3 0.3781879 0.2949303 0.3268818
    ## 4 0.4517007 0.3079192 0.2403800
    ## 5 0.1939238 0.2145603 0.5915159
    ## 6 0.3931465 0.2984564 0.3083971

------------------------------------------------------------------------

$\bullet$ **Example:**

Assume we want to predict the probabilities for each drive type if the
horse power equal to 200.

The probability that the vehicle will have AWD drive type, conditional
on the horse power equal to 200 will be:
$$\mathrm{P}(Drive=AWD \mid HP=200) = \frac{e^{1.1774-0.0054*200}}{1+e^{1.1774-0.0054*200}+e^{4.9953-0.0194*200}}$$
$$\approx 0.2139$$

We can calculate the probability for the other two classes:
$$\mathrm{P}(Drive=FWD \mid HP=200) = \frac{e^{4.9953-0.0194*200}}{1+e^{1.1774-0.0054*200}+e^{4.9953-0.0194*200}}$$
$$\approx 0.592 $$

$$\mathrm{P}(Drive=RWD \mid HP=200) = \frac{1}{1+e^{1.1774-0.0054*200}+e^{4.9953-0.0194*200}}$$
$$\approx 0.1941$$

The sum of the probabilities is: $0.2139+0.592+0.1941=1$.

That means, given a vehicle with a horse power of 200, the probability
that it has AWD or FWD or RWD is approximately 21%, 59% or 19%,
respectively.

We can verify that using `R`:

``` r
predict(model, dt_cars[HP==200],type="prob")
```

    ##          RWD       AWD       FWD
    ## 1  0.1939238 0.2145603 0.5915159
    ## 2  0.1939238 0.2145603 0.5915159
    ## 3  0.1939238 0.2145603 0.5915159
    ## 4  0.1939238 0.2145603 0.5915159
    ## 5  0.1939238 0.2145603 0.5915159
    ## 6  0.1939238 0.2145603 0.5915159
    ## 7  0.1939238 0.2145603 0.5915159
    ## 8  0.1939238 0.2145603 0.5915159
    ## 9  0.1939238 0.2145603 0.5915159
    ## 10 0.1939238 0.2145603 0.5915159
    ## 11 0.1939238 0.2145603 0.5915159
    ## 12 0.1939238 0.2145603 0.5915159
    ## 13 0.1939238 0.2145603 0.5915159
    ## 14 0.1939238 0.2145603 0.5915159
    ## 15 0.1939238 0.2145603 0.5915159
    ## 16 0.1939238 0.2145603 0.5915159
    ## 17 0.1939238 0.2145603 0.5915159

The manual calculation gives accuracy up to two decimal places.

------------------------------------------------------------------------

$\bullet$ **Plot the horse power against the class probabilities:**

``` r
#first we merge the probabilities with the dataset.
dt_cars<-cbind(dt_cars, probability = fitted(model))

#have a look on the variables.
ls(dt_cars)
```

    ##  [1] "AWD"             "CityMPG"         "Cyl"             "Dealer Cost"    
    ##  [5] "Drive"           "EngineSize"      "HP"              "HwyMPG"         
    ##  [9] "Length"          "Minivan"         "Pickup"          "probability.AWD"
    ## [13] "probability.FWD" "probability.RWD" "RetailPrice"     "RWD"            
    ## [17] "SportsCar"       "Standard"        "SUV"             "type"           
    ## [21] "VehicleName"     "Wagon"           "Weight"          "WheelBase"      
    ## [25] "Width"

``` r
library(ggplot2)

#plot horse power against the 3 class probabilities.
ggplot(dt_cars,aes(x=HP))+
  geom_line(aes(y=probability.AWD,color="AWD"))+
  geom_line(aes(y=probability.FWD,color="FWD"))+
  geom_line(aes(y=probability.RWD,color="RWD"))+
  labs(y="Probability") +
  scale_color_manual(name="Drive",values=c(AWD="red",FWD="blue",RWD="black"))+
  facet_grid(type~.) #writing the name of the variable before "~" will create the grid in a horizontal format.
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/mnl/unnamed-chunk-7-1.png?raw=true)<!-- -->

------------------------------------------------------------------------

------------------------------------------------------------------------

$\bullet$ **Classification Tree:**

``` r
library(rpart)
library(rpart.plot)

#classification tree for Drive on HP.
model_tree<-rpart(Drive~HP,data=dt_cars)

#plot the tree.
prp(model_tree)
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/mnl/unnamed-chunk-8-1.png?raw=true)<!-- -->

``` r
library(parttree)

#plot with HP on the x-axis, and its density on the y-axis, with vertical lines that indicate the splits, and the area between the splits are colored by the drive type.
ggplot(dt_cars,aes(x=HP))+
  geom_density()+
  geom_parttree(data=model_tree,aes(fill=Drive),alpha=0.5)+
  labs(y="Density")
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/mnl/unnamed-chunk-8-2.png?raw=true)<!-- -->

------------------------------------------------------------------------

$\bullet$ **Comparing between the classification tree chart and the MNL
chart:**

In the classification tree chart, the first region (lower levels of
horse power) is green, representing FWD. That is consistent with the
predicted probability in the MNL chart: the blue line in the MNL chart
(FWD), for the low levels of horse power, is higher than the other drive
types (e.g. higher probability).

In the classification tree chart, the last region (higher levels of
horse power) is blue, representing RWD. That is consistent, as well,
with the predicted probability in the MNL chart: the black line in the
MNL chart (RWD), for higher levels of horse power, is higher than the
other drive types (e.g. higher probability)

------------------------------------------------------------------------

------------------------------------------------------------------------

$\bullet$ **Regression Tree:**

``` r
#regression tree for CityMPG on HP.
reg_tree<-rpart(CityMPG~HP,data=dt_cars)

#plot the tree.
rpart.plot(reg_tree)
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/mnl/unnamed-chunk-9-1.png?raw=true)<!-- -->

The average CityMPG for the whole data is 20.

For horse power greater than or equal 141, the average CityMPG is 19,
and that represents 85% of the data, and so on.

As we go down in the tree, the conditions become stricter, till it ends
with five leafs at the end, which combined represents 100% of the data.

1)  If $HP \ge 273$, the average CityMPG = 16, and this represents 21%
    of the data.

2)  If $183 \le HP < 273$, the average CityMPG =19, and this represents
    46% of the data.

3)  If $141 \le HP <183$, the average CityMPG = 21, and this represents
    18% of the data.

4)  If $113 \le HP <141$, the average CityMPG=27, and this represents
    11% of the data.

5)  If $HP<113$, the average CityMPG=34, and this represents 4% of the
    data.

We can see the conditions using `rpart.rules`

``` r
rpart.rules(reg_tree)
```

    ##  CityMPG                      
    ##       16 when HP >=        273
    ##       19 when HP is 183 to 273
    ##       21 when HP is 141 to 183
    ##       27 when HP is 113 to 141
    ##       34 when HP <  113

``` r
#extract the average CityMPG from the leafs, by extracting the data frame for the fitted "rpart" tree. 
reg_tree$frame
```

    ##      var   n  wt        dev     yval  complexity ncompete nsurrogate
    ## 1     HP 412 412 11214.9199 20.09951 0.502531728        0          0
    ## 2     HP 350 350  2548.8571 18.54286 0.068468519        0          0
    ## 4     HP 274 274  1441.5803 17.76277 0.032789694        0          0
    ## 8 <leaf>  85  85   324.8941 16.03529 0.001992630        0          0
    ## 9 <leaf> 189 189   748.9524 18.53968 0.001957478        0          0
    ## 5 <leaf>  76  76   339.4079 21.35526 0.007392736        0          0
    ## 3     HP  62  62  3030.2097 28.88710 0.064373128        0          0
    ## 6 <leaf>  44  44   427.1591 26.70455 0.004512745        0          0
    ## 7 <leaf>  18  18  1881.1111 34.22222 0.010000000        0          0

``` r
#From it, we can create another data frame that contains the data points of the leafs and "yval". 
#"yval" is for y-values, which are the the fitted values of CityMPG/the average of CityMPG.
City<-data.frame(CityMPG=c(reg_tree$frame[4,5],reg_tree$frame[5,5],reg_tree$frame[6,5],reg_tree$frame[8,5],reg_tree$frame[9,5])) #the numbers in the squared brackets are for the number of row and the number of column, respectively.


#plot the data, the linear regression line, and horizontal lines that display the average CityMPG per leaf.
ggplot(dt_cars,aes(x=HP,y=CityMPG))+
  geom_point()+
  geom_smooth(formula=y~x,method = "lm")+ #"lm" is for linear model.
  geom_hline(yintercept = City$CityMPG,color="red",alpha=0.3)+ #"alpha" is for the intensity of the color. The higher, the more intense (less transparent) the color will be. 
  labs(y="City MPG")+
  scale_y_continuous(breaks = round(sort(c(seq(min(dt_cars$CityMPG,na.rm=TRUE), max(dt_cars$CityMPG,na.rm=TRUE), by=15), City$CityMPG)))) #to display the values of the average CityMPG by using "breaks", but first I round the numbers to the nearest integer, and then sort all the possible values for CityMPG by creating a sequence from its minimum to its maximum alongside the average CityMPG per leaf, and "by" is the difference between each break.
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/mnl/unnamed-chunk-11-1.png?raw=true)<!-- -->

------------------------------------------------------------------------

------------------------------------------------------------------------

**References:**

$\bullet$ Hosmer, D. W., Lemeshow, S. (2000): *Applied Logistic
Regression*. 2<sup>nd</sup> ed., John Wiley & Sons, Inc.:
Wiley-Interscience Publication, Wiley Series in Probability and
Statistics.

[^1]: See: Hosmer and Lemeshow, 2000, pp. 260-
