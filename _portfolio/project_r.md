---
title: "Simple forecast of CO<sub>2</sub> emissions"
excerpt: "Short description"
collection: portfolio
---
  

Today I wanted to look into how CO<sub>2</sub> emissions changed over
time, not in a rigorous way but just to have a glimpse about where are
we going in controlling the level of emissions. So, I found a dataset of
carbon dioxide emissions per capita, in metric tons, published by the
World Bank
[here](https://data.worldbank.org/indicator/EN.ATM.CO2E.PC?view=chart).
The dataset considers CO<sub>2</sub> emissions as those stemming from
the burning of fossil fuels and the manufacture of cement. They include
carbon dioxide produced during consumption of solid, liquid, and gas
fuels and gas flaring.

Let’s start!

First load the packages we need:

``` r
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(tseries)
library(forecast)
library(reshape2)
library(janitor)

#import the dataset. I renamed the file as you can see.
emission<-read_xls("carbondioxideemmision.xls")

#now let's see the first small part of the data
emission[1:7,1:7] #alternatively, you can use head() function to review the first parts of the data.
```

    ## # A tibble: 7 × 7
    ##   `Data Source`     `World Development Indicators` ...3  ...4  ...5  ...6  ...7 
    ##   <chr>             <chr>                          <chr> <chr> <chr> <chr> <chr>
    ## 1 Last Updated Date 44274                          <NA>  <NA>  <NA>  <NA>  <NA> 
    ## 2 <NA>              <NA>                           <NA>  <NA>  <NA>  <NA>  <NA> 
    ## 3 Country Name      Country Code                   Indi… Indi… 1960  1961  1962 
    ## 4 Aruba             ABW                            CO2 … EN.A… 204.… 208.… 226.…
    ## 5 Afghanistan       AFG                            CO2 … EN.A… 0.04… 0.05… 0.07…
    ## 6 Angola            AGO                            CO2 … EN.A… 0.10… 0.08… 0.21…
    ## 7 Albania           ALB                            CO2 … EN.A… 1.25… 1.37… 1.43…

Before doing anything, we have to clean the data first.

``` r
#remove the first two rows, since they contain nothing.
emission<-emission[-c(1:2),]

#the first row contains the names of columns, yet the columns are not named properly. Change the columns names to be as the first row
colnames(emission)<-emission[1,]

#after we renamed the columns by the first row, we don't need the first row anymore, so let's delete it.
emission<-emission[-c(1),]
```

Now let’s see again:

``` r
emission[1:5,]
```

    ## # A tibble: 5 × 65
    ##   Country Na…¹ Count…² Indic…³ Indic…⁴ `1960` `1961` `1962` `1963` `1964` `1965`
    ##   <chr>        <chr>   <chr>   <chr>   <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 Aruba        ABW     CO2 em… EN.ATM… 204.6… 208.8… 226.1… 214.8… 207.6… 185.2…
    ## 2 Afghanistan  AFG     CO2 em… EN.ATM… 0.046… 0.053… 0.073… 0.074… 0.086… 0.101…
    ## 3 Angola       AGO     CO2 em… EN.ATM… 0.100… 0.082… 0.210… 0.202… 0.213… 0.205…
    ## 4 Albania      ALB     CO2 em… EN.ATM… 1.258… 1.374… 1.439… 1.181… 1.111… 1.166…
    ## 5 Andorra      AND     CO2 em… EN.ATM… <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ## # … with 55 more variables: `1966` <chr>, `1967` <chr>, `1968` <chr>,
    ## #   `1969` <chr>, `1970` <chr>, `1971` <chr>, `1972` <chr>, `1973` <chr>,
    ## #   `1974` <chr>, `1975` <chr>, `1976` <chr>, `1977` <chr>, `1978` <chr>,
    ## #   `1979` <chr>, `1980` <chr>, `1981` <chr>, `1982` <chr>, `1983` <chr>,
    ## #   `1984` <chr>, `1985` <chr>, `1986` <chr>, `1987` <chr>, `1988` <chr>,
    ## #   `1989` <chr>, `1990` <chr>, `1991` <chr>, `1992` <chr>, `1993` <chr>,
    ## #   `1994` <chr>, `1995` <chr>, `1996` <chr>, `1997` <chr>, `1998` <chr>, …

``` r
#since we have "years" as variables (columns), we can transpose the data so that countries will be variables and years as rows. I'll tell you later why we did that. 
emission<-as.data.frame(t(emission))

#again column names are not properly named, so we change them as before.
colnames(emission)<-emission[1,]

#the first four rows are no use for us, we can remove them. The first row contains country names, which we had already used to name our columns, the remaining rows contain country code, indicator name, and indicator code, respectively, and we'll not use any of them.
emission<-emission[-c(1:4),]

#now let's have a look
emission[1:7,1:7]
```

    ##                   Aruba          Afghanistan               Angola
    ## 1960 204.62037224917452 0.046056712629903414   0.1008353356494021
    ## 1961 208.82281106822035 0.053588835050455808 0.082203796747050334
    ## 1962 226.11807914628724 0.073720830832381873  0.21053147709234077
    ## 1963 214.80037040303375 0.074160724829865854  0.20273730345395635
    ## 1964 207.61577710758871 0.086173614368552767  0.21356034931902876
    ## 1965 185.20395746164576  0.10128491249779034  0.20589092585307864
    ## 1966 172.11995148574894  0.10739888092545179  0.26894143686775823
    ##                 Albania Andorra          Arab World United Arab Emirates
    ## 1960 1.2581949278965689    <NA> 0.60744755675365358  0.11903525287281698
    ## 1961 1.3741860465116278    <NA> 0.66063794460729808  0.10914123576332393
    ## 1962 1.4399559637916719    <NA> 0.72494431853622876  0.16353306337965359
    ## 1963 1.1816811441597486    <NA> 0.85056679990052364  0.17583313354111724
    ## 1964 1.1117419596667282    <NA> 0.96947620360177611  0.13282478140235732
    ## 1965 1.1660990427345477    <NA>  1.1352713004209469  0.14681996836984593
    ## 1966 1.3330554645866206    <NA>  1.2482565638723149  0.16045531829774465

``` r
#now before playing with these numbers let's see if R recognize them as numbers. Let's take the first country as an example.
class(emission$Aruba)
```

    ## [1] "character"

``` r
#R doesn't recognize the values as numbers. So we can use lapply function to declare all of them as numbers to R. Now here comes why we transposed the data before. The idea is to not have a country column, so I'd declare all columns as numbers to R without being concerned about a character column (the country column)  
emission[]<-lapply(emission,function(x) as.numeric(as.character(x)))

#then let's remove the countries that we don't have any observation about it.
emission<-emission[, colSums(is.na(emission))<nrow(emission)]

#the "year" is not recognized as a column by R yet. Let's declare it.
setDT(emission,keep.rownames = "year")

#let's have a quick look
emission[1:5,1:5]
```

    ##    year    Aruba Afghanistan    Angola  Albania
    ## 1: 1960 204.6204  0.04605671 0.1008353 1.258195
    ## 2: 1961 208.8228  0.05358884 0.0822038 1.374186
    ## 3: 1962 226.1181  0.07372083 0.2105315 1.439956
    ## 4: 1963 214.8004  0.07416072 0.2027373 1.181681
    ## 5: 1964 207.6158  0.08617361 0.2135603 1.111742

``` r
emission[56:61,249:254]
```

    ##       Samoa   Kosovo Yemen, Rep. South Africa    Zambia  Zimbabwe
    ## 1: 1.212776 4.702186   0.4970911     8.376655 0.2854279 0.8916239
    ## 2: 1.262955 5.031520   0.3904796     8.480658 0.3141829 0.7827769
    ## 3:       NA       NA          NA           NA        NA        NA
    ## 4:       NA       NA          NA           NA        NA        NA
    ## 5:       NA       NA          NA           NA        NA        NA
    ## 6:       NA       NA          NA           NA        NA        NA

``` r
#it seems the last four years don't have any observations, let's remove them if they don't have any observations.
emission<- emission[rowSums(is.na(emission)) != ncol(emission), ]
emission[56:61,249:254]
```

    ##       Samoa   Kosovo Yemen, Rep. South Africa    Zambia  Zimbabwe
    ## 1: 1.212776 4.702186   0.4970911     8.376655 0.2854279 0.8916239
    ## 2: 1.262955 5.031520   0.3904796     8.480658 0.3141829 0.7827769
    ## 3:       NA       NA          NA           NA        NA        NA
    ## 4:       NA       NA          NA           NA        NA        NA
    ## 5:       NA       NA          NA           NA        NA        NA
    ## 6:       NA       NA          NA           NA        NA        NA

``` r
#then they might be just empty
emission<-emission[!apply(emission == "", 1, all), ]
emission[56:61,249:254]
```

    ##       Samoa   Kosovo Yemen, Rep. South Africa    Zambia  Zimbabwe
    ## 1: 1.212776 4.702186   0.4970911     8.376655 0.2854279 0.8916239
    ## 2: 1.262955 5.031520   0.3904796     8.480658 0.3141829 0.7827769
    ## 3:       NA       NA          NA           NA        NA        NA
    ## 4:       NA       NA          NA           NA        NA        NA
    ## 5:       NA       NA          NA           NA        NA        NA
    ## 6:       NA       NA          NA           NA        NA        NA

``` r
#again nothing happened. let's try another way
emission<-janitor::remove_empty(emission,which=c("rows"))
emission[56:61,249:254]
```

    ##       Samoa   Kosovo Yemen, Rep. South Africa    Zambia  Zimbabwe
    ## 1: 1.212776 4.702186   0.4970911     8.376655 0.2854279 0.8916239
    ## 2: 1.262955 5.031520   0.3904796     8.480658 0.3141829 0.7827769
    ## 3:       NA       NA          NA           NA        NA        NA
    ## 4:       NA       NA          NA           NA        NA        NA
    ## 5:       NA       NA          NA           NA        NA        NA
    ## 6:       NA       NA          NA           NA        NA        NA

``` r
#again nothing happened! we'll get rid of that problem later.

#let's transform our data to long format so that we'd have the countries as a column altogether, resulting in a panel data
emission<-melt(emission, id.vars="year",variable.name = "country")
head(emission)
```

    ##   year country    value
    ## 1 1960   Aruba 204.6204
    ## 2 1961   Aruba 208.8228
    ## 3 1962   Aruba 226.1181
    ## 4 1963   Aruba 214.8004
    ## 5 1964   Aruba 207.6158
    ## 6 1965   Aruba 185.2040

``` r
#now let's start playing! first let's see how many countries or regions do we have. Keep in mind that the world bank usually aggregate the data based on the region or based on income, so the number here will be higher than the no. of countries.
length(unique(emission$country))
```

    ## [1] 253

``` r
#let's go with the world values. first filter the data so that we have only the values for the world as a whole. Analogously, you can do the same for any country or countries as you want.
world<-dplyr::filter(emission, country == "World")

#remember that when we declared our values as numeric to R, the "years" column was not yet part of our data. it's still as a character 
class(world$year)
```

    ## [1] "character"

``` r
#declare it as numeric.
world$year<-as.numeric(world$year)
```

``` r
#let's have a look
head(world)
```

    ##   year country    value
    ## 1 1960   World 3.039297
    ## 2 1961   World 2.986053
    ## 3 1962   World 3.031556
    ## 4 1963   World 3.135074
    ## 5 1964   World 3.235182
    ## 6 1965   World 3.340645

``` r
#create a binary variable for countries so that it'll be TRUE if the emissions are higher than the emission average over time and FALSE otherwise. 
world<- world %>%
  mutate(above_avg=ifelse(value>mean(world$value,na.rm = TRUE),T,F))

#let's remove the missing values, which was a problem before for us, but now it's easier since we have a time series data.
world<-world[!is.na(world$value), ]

#we can plot it and see!
ggplot(mapping=aes(x=year,y=value),data=world)+geom_line()+geom_point(aes(color=above_avg))+scale_x_continuous(breaks=seq(1960,2016,by=5))+scale_y_continuous(breaks = seq(2,5,by=0.2))+labs(x="year",y="CO2 emissions (metric tons per capita)")+theme(legend.position = "none",axis.title = element_text(size=10))+geom_hline(yintercept = mean(world$value,na.rm=TRUE))+scale_color_manual(values=c("TRUE"="red","FALSE"="black"))+annotate("text",x=2012,y=mean(world$value,na.rm=TRUE),vjust=2, label="Average emission")
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-7.png?raw=true)<!-- -->

The emissions varied between
![3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;3 "3")
metric tons per capita and
![4.75](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;4.75 "4.75")
metric tons per capita over the period
![1960-2016](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1960-2016 "1960-2016").
It started to increase over time. The sharp decline was in the late
![1980s](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1980s "1980s")
with steady levels in the
![1990s](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1990s "1990s")
until early
![2000s](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;2000s "2000s")
followed by a sharp increase again.

But we may not be able to properly interpret the real difference between
![3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;3 "3")
metric tons per capita and
![3.5](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;3.5 "3.5")
metric tons per capita, so we can see the annual percentage change of
the emissions over the same period.

``` r
#create a new variable to capture the annual percentage change.
#you can also use this equation. It will give the same result. world<-world %>%+mutate(annual_change=((value/lag(value))-1)*100)
world$annual_change<-(((world$value-dplyr::lag(world$value))/dplyr::lag(world$value))*100)
head(world)
```

    ##   year country    value above_avg annual_change
    ## 1 1960   World 3.039297     FALSE            NA
    ## 2 1961   World 2.986053     FALSE     -1.751856
    ## 3 1962   World 3.031556     FALSE      1.523857
    ## 4 1963   World 3.135074     FALSE      3.414676
    ## 5 1964   World 3.235182     FALSE      3.193167
    ## 6 1965   World 3.340645     FALSE      3.259891

``` r
#now let's plot with the new variable. to determine the scale for x-axis and y-axis, I got the minimum and maximum for the years and for the percentages, using min() and max() functions. Note that when you use any of them always remove the missing values: na.rm=TRUE
ggplot(mapping=aes(x=year,y=annual_change),data=world)+geom_line()+geom_point()+scale_x_continuous(breaks=seq(1960,2016,by=5))+scale_y_continuous(breaks = seq(-7,8,by=2))+labs(x="year",y="Annual Percentage Change of CO2 emissions (metric tons per capita)")+theme(legend.position = "none",axis.title = element_text(size=8))
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-8.png?raw=true)<!-- -->

We can see that the highest change was in
![1970](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1970 "1970")
with more than
![7\\%](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;7%5C%25 "7\%")
increase than
![1969](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1969 "1969").
The biggest success was in
![1992](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1992 "1992")
when the emissions decreased by more than
![6\\%](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;6%5C%25 "6\%")
than the year before. As you can tell now, we only capture the change
with only one year difference
(lag=![1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1 "1")),
but we might have different numbers if we calculated over many years.

For example: let’s see the change from
![1960](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1960 "1960")
to
![2016](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;2016 "2016").
First let’s define how to calculate it: such that
![r](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r "r")
is the rate of change, and
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")
is the time period.

Therefore,

Let’s calculate it:

``` r
((((world[1,3])/(world[57,3]))^(1/(2016-1960)))-1)*100
```

    ## [1] -0.7199824

Between
![1960](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1960 "1960")
and
![2016](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;2016 "2016"),
the metric tons per capita emissions of CO<sub>2</sub> decreased by
![0.72\\%](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;0.72%5C%25 "0.72\%")

## **Forecasting:**

Let’s try to make a forecast for the upcoming
![10](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;10 "10")
years. First, for the theory behind forecasting, see, for example,
(Hamilton,
![1994](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1994 "1994"),
pp. ![72](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;72 "72")-).
Also note that all forecasting techniques have its own limitations,
which you can look for it elsewhere.

``` r
#first declare the data for R as time series.
world_ts<-ts(world[,3],start=c(1960),end=c(2016),frequency = 1)

head(world_ts,n=20)
```

    ## Time Series:
    ## Start = 1960 
    ## End = 1979 
    ## Frequency = 1 
    ##  [1] 3.039297 2.986053 3.031556 3.135074 3.235182 3.340645 3.439732 3.490265
    ##  [9] 3.606479 3.771645 4.037076 4.119826 4.232276 4.375075 4.287073 4.227351
    ## [17] 4.374219 4.438372 4.529924 4.571860

We’re going to use [Exponential
Smoothing](https://mran.microsoft.com/web/packages/forecast/vignettes/JSS2008.pdf).

``` r
ets(world_ts)
```

    ## ETS(M,Ad,N) 
    ## 
    ## Call:
    ##  ets(y = world_ts) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.9891 
    ##     beta  = 0.4135 
    ##     phi   = 0.8 
    ## 
    ##   Initial states:
    ##     l = 3.0286 
    ##     b = 0.0094 
    ## 
    ##   sigma:  0.0244
    ## 
    ##       AIC      AICc       BIC 
    ## -26.11861 -24.43861 -13.86030

Let’s have a look on what does that mean.
![ETS(M,Ad,N)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;ETS%28M%2CAd%2CN%29 "ETS(M,Ad,N)")
specifies the exponential smoothing method.
![M](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;M "M")
for multiplicative error over time,
![Ad](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Ad "Ad")
for the trend component which additive damped,
![N](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N "N")
for the seasonal component which is None, as expected that our data has
no seasonal component.

The damped trend method tries to depress the trend to be flat at some
point in the future. The rate of that “depressing” is
![\phi](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cphi "\phi"),
which is given by
![0.8](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;0.8 "0.8").
![\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
and
![\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")
are the smoothing parameters for the level and for the trend,
respectively, Sigma,
![\sigma](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma "\sigma"),
is the standard deviation of residuals,
![AIC](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;AIC "AIC")
is the Akaike Information Criterion which gives an estimation of how
well the model fits the data, the lower the better.
![AICc](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;AICc "AICc")
is the corrected Akaike Information Criterion to correct for the sample
size and the number of parameters.
![BIC](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;BIC "BIC")
is Bayesian Information Criteria. For more on
![AIC](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;AIC "AIC")
and
![BIC](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;BIC "BIC"),
see: (Claeskens and Hjort,
![2008](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;2008 "2008"),
pp. ![22](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;22 "22")-![98](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;98 "98")).

``` r
#let's see the accuracy of our model.
accuracy(ets(world_ts))
```

    ##                       ME       RMSE        MAE       MPE     MAPE      MASE
    ## Training set 0.008622907 0.09729994 0.07628865 0.2485306 1.856737 0.8768713
    ##                     ACF1
    ## Training set -0.02579842

![ME](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;ME "ME")
is the Mean Error,
![RMSE](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;RMSE "RMSE")
is the Root Mean Squared Error,
![MAE](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;MAE "MAE")
is the Mean Absolute Error,
![MPE](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;MPE "MPE")
is the Mean Percentage Error,
![MAPE](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;MAPE "MAPE")
is the Mean Absolute Percentage Error,
![MASE](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;MASE "MASE")
is the Mean Absolute Scaled Error, and
![ACF1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;ACF1 "ACF1")
is the AutoCorrelation of Error at lag
![1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1 "1").

``` r
#let's forecast!
#it will show the intervals at 80% and 95% confidence. you can change the level of confidence if you want.
forecast(ets(world_ts)) 
```

    ##      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## 2017       4.517768 4.376439 4.659096 4.301624 4.733911
    ## 2018       4.487264 4.253763 4.720765 4.130155 4.844373
    ## 2019       4.462861 4.140282 4.785441 3.969518 4.956204
    ## 2020       4.443339 4.034156 4.852522 3.817548 5.069130
    ## 2021       4.427721 3.934747 4.920696 3.673782 5.181661
    ## 2022       4.415227 3.841547 4.988907 3.537859 5.292595
    ## 2023       4.405232 3.754050 5.056413 3.409335 5.401128
    ## 2024       4.397235 3.671748 5.122723 3.287698 5.506772
    ## 2025       4.390838 3.594149 5.187527 3.172408 5.609269
    ## 2026       4.385721 3.520790 5.250651 3.062924 5.708517

Let’s get to the final result:

``` r
#to summarize what we got so far
summary(forecast(ets(world_ts)))
```

    ## 
    ## Forecast method: ETS(M,Ad,N)
    ## 
    ## Model Information:
    ## ETS(M,Ad,N) 
    ## 
    ## Call:
    ##  ets(y = world_ts) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.9891 
    ##     beta  = 0.4135 
    ##     phi   = 0.8 
    ## 
    ##   Initial states:
    ##     l = 3.0286 
    ##     b = 0.0094 
    ## 
    ##   sigma:  0.0244
    ## 
    ##       AIC      AICc       BIC 
    ## -26.11861 -24.43861 -13.86030 
    ## 
    ## Error measures:
    ##                       ME       RMSE        MAE       MPE     MAPE      MASE
    ## Training set 0.008622907 0.09729994 0.07628865 0.2485306 1.856737 0.8768713
    ##                     ACF1
    ## Training set -0.02579842
    ## 
    ## Forecasts:
    ##      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## 2017       4.517768 4.376439 4.659096 4.301624 4.733911
    ## 2018       4.487264 4.253763 4.720765 4.130155 4.844373
    ## 2019       4.462861 4.140282 4.785441 3.969518 4.956204
    ## 2020       4.443339 4.034156 4.852522 3.817548 5.069130
    ## 2021       4.427721 3.934747 4.920696 3.673782 5.181661
    ## 2022       4.415227 3.841547 4.988907 3.537859 5.292595
    ## 2023       4.405232 3.754050 5.056413 3.409335 5.401128
    ## 2024       4.397235 3.671748 5.122723 3.287698 5.506772
    ## 2025       4.390838 3.594149 5.187527 3.172408 5.609269
    ## 2026       4.385721 3.520790 5.250651 3.062924 5.708517

``` r
#plot our forecast
plot(forecast(ets(world_ts)))
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-14-1.png?raw=true)<!-- -->

  

It seems that, on average, the emissions won’t increase significantly,
which we hope to be true!

**Keep in mind that we looked into the emissions data only without
controlling for any variable that might affect the level of emissions**.

Finally, I want to say that I didn’t aim to be rigorous. I just wanted
quickly, in my free time, to look into something that interests me and
sharing it, so it might open a door for a new idea for someone else or
for me in the future.

## **References:**

<p style="font-family: times, serif; font-size:11pt; font-style:italic">

- Claeskens, G., Hjort, N. L. (2008): *Model Selection and Model
  Averaging*. Cambridge: Cambridge University Press.

- Hamilton, J. D. (1994): *Time Series Analysis*. New Jersey: Princeton
  University Press.

- Hyndman, R. J., Khandakar, Y. (2008): “Automatic Time Series
  Forecasting: the forecast Package for R”. *Journal of Statistical
  Software*, Vol.(27)(3), 1-22.

</p>
