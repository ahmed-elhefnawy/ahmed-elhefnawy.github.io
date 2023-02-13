---
title: "Gasoline demand in Germany"
excerpt: "Short description"
collection: portfolio
---
  

We try to look into the motor gasoline demand in Germany over the period
![1960-1978](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1960-1978 "1960-1978").

The dataset is available in the package `AER` under the name `OECDGas`.
It’s also available in the package `plm` under a different name.

You can use `?OECDGas` after loading `AER` package to get a description
of the data. The variables in our data are in logarithmic form.
We will also use the population data available from the [World
Bank](https://data.worldbank.org/indicator/SP.POP.TOTL) since the gas
consumption in our data is per car, not aggregate consumption.

``` r
#load the packages we need.
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(AER)
```

``` r
#loading the dataset.
data("OECDGas")

head(OECDGas)
```

      country year      gas    income      price      cars
    1 Austria 1960 4.173244 -6.474277 -0.3345476 -9.766840
    2 Austria 1961 4.100989 -6.426006 -0.3513276 -9.608622
    3 Austria 1962 4.073177 -6.407308 -0.3795177 -9.457257
    4 Austria 1963 4.059509 -6.370679 -0.4142514 -9.343155
    5 Austria 1964 4.037689 -6.322247 -0.4453354 -9.237739
    6 Austria 1965 4.033983 -6.294668 -0.4970607 -9.123903

``` r
#filter our data to include only Germany.
gas<-dplyr::filter(OECDGas,country=="Germany")

#import the population data. I renamed the file as you can see.
pop<-read_xls("population.xls")

head(pop)
```

    # A tibble: 6 × 65
      Data Sou…¹ World…² ...3  ...4  ...5  ...6  ...7  ...8  ...9  ...10 ...11 ...12
      <chr>      <chr>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    1 Last Upda… 44274   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA> 
    2 <NA>       <NA>    <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA> 
    3 Country N… Countr… Indi… Indi… 1960  1961  1962  1963  1964  1965  1966  1967 
    4 Aruba      ABW     Popu… SP.P… 54211 55438 56225 56695 57032 57360 57715 58055
    5 Afghanist… AFG     Popu… SP.P… 8996… 9169… 9351… 9543… 9744… 9956… 1017… 1039…
    6 Angola     AGO     Popu… SP.P… 5454… 5531… 5608… 5679… 5735… 5770… 5781… 5774…
    # … with 53 more variables: ...13 <chr>, ...14 <chr>, ...15 <chr>, ...16 <chr>,
    #   ...17 <chr>, ...18 <chr>, ...19 <chr>, ...20 <chr>, ...21 <chr>,
    #   ...22 <chr>, ...23 <chr>, ...24 <chr>, ...25 <chr>, ...26 <chr>,
    #   ...27 <chr>, ...28 <chr>, ...29 <chr>, ...30 <chr>, ...31 <chr>,
    #   ...32 <chr>, ...33 <chr>, ...34 <chr>, ...35 <chr>, ...36 <chr>,
    #   ...37 <chr>, ...38 <chr>, ...39 <chr>, ...40 <chr>, ...41 <chr>,
    #   ...42 <chr>, ...43 <chr>, ...44 <chr>, ...45 <chr>, ...46 <chr>, …

``` r
#we can remove the first two rows since they are empty.
pop<-pop[-c(1:2),]

#rename the columns as the values of the first row.
colnames(pop)<-pop[1,]

#remove the first row since we don't need it anymore.
pop<-pop[-c(1),]

#remove the second, third, and fourth columns as they contain the country code, the indicator name, the indicator code respectively and we won't use them.
pop<-pop[,-c(2:4)]

head(pop)
```

    # A tibble: 6 × 62
      Countr…¹ `1960` `1961` `1962` `1963` `1964` `1965` `1966` `1967` `1968` `1969`
      <chr>    <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    1 Aruba    54211  55438  56225  56695  57032  57360  57715  58055  58386  58726 
    2 Afghani… 89969… 91694… 93514… 95432… 97447… 99563… 10174… 10399… 10637… 10893…
    3 Angola   54549… 55314… 56085… 56794… 57350… 57705… 57812… 57742… 57716… 58032…
    4 Albania  16088… 16598… 17113… 17626… 18141… 18647… 19145… 19655… 20222… 20816…
    5 Andorra  13411  14375  15370  16412  17469  18549  19647  20758  21890  23058 
    6 Arab Wo… 92197… 94724… 97334… 10003… 10283… 10573… 10875… 11189… 11513… 11843…
    # … with 51 more variables: `1970` <chr>, `1971` <chr>, `1972` <chr>,
    #   `1973` <chr>, `1974` <chr>, `1975` <chr>, `1976` <chr>, `1977` <chr>,
    #   `1978` <chr>, `1979` <chr>, `1980` <chr>, `1981` <chr>, `1982` <chr>,
    #   `1983` <chr>, `1984` <chr>, `1985` <chr>, `1986` <chr>, `1987` <chr>,
    #   `1988` <chr>, `1989` <chr>, `1990` <chr>, `1991` <chr>, `1992` <chr>,
    #   `1993` <chr>, `1994` <chr>, `1995` <chr>, `1996` <chr>, `1997` <chr>,
    #   `1998` <chr>, `1999` <chr>, `2000` <chr>, `2001` <chr>, `2002` <chr>, …

``` r
#transform our data from wide to long format.
pop<-gather(pop,year,population,-1)

#change the name of the first column.
colnames(pop)[1]<-"country"

#filter our data to include only Germany.
pop<-dplyr::filter(pop,country=="Germany")

#filter our data to exclude the years after 1978.
pop<-dplyr::filter(pop,year<=1978)

#merge the population data with our original dataset.
gas$population<-pop$population

head(gas)
```

        country year      gas    income      price      cars population
    96  Germany 1960 3.916953 -6.159837 -0.1859108 -9.342481   72814900
    97  Germany 1961 3.885345 -6.120923 -0.2309538 -9.183841   73377632
    98  Germany 1962 3.871484 -6.094258 -0.3438417 -9.037280   74025784
    99  Germany 1963 3.848782 -6.068361 -0.3746467 -8.913630   74714353
    100 Germany 1964 3.868993 -6.013442 -0.3996526 -8.811013   75318337
    101 Germany 1965 3.861049 -5.966469 -0.4398783 -8.711888   75963695

``` r
#let's see if R recognizes the population values as numbers.
class(gas$population)
```

    [1] "character"

``` r
#declare them as numeric.
gas$population<-as.numeric(gas$population)

#calculate aggregate gas consumption. 
#"gas" variable is the logarithm of gas consumption per car, "cars" variable is the logarithm of the stock of cars per capita, and that's why we needed the population data.
gas$gasq<-exp(gas$gas)*exp(gas$cars)*gas$population

head(gas)
```

        country year      gas    income      price      cars population     gasq
    96  Germany 1960 3.916953 -6.159837 -0.1859108 -9.342481   72814900 320585.3
    97  Germany 1961 3.885345 -6.120923 -0.2309538 -9.183841   73377632 366822.9
    98  Germany 1962 3.871484 -6.094258 -0.3438417 -9.037280   74025784 422577.4
    99  Germany 1963 3.848782 -6.068361 -0.3746467 -8.913630   74714353 471811.9
    100 Germany 1964 3.868993 -6.013442 -0.3996526 -8.811013   75318337 537785.1
    101 Germany 1965 3.861049 -5.966469 -0.4398783 -8.711888   75963695 594174.0

The linear demand function is then: Where
![Q_g](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Q_g "Q_g")
is the motor gasoline quantity consumed,
![\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
and
![\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")
are the intercept and the slope, respectively, and
![P_g](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;P_g "P_g")
is the real motor gasoline price.

``` r
#estimate the linear relationship between quantity and price.
lm(gasq/1000~exp(price),data=gas)
```


    Call:
    lm(formula = gasq/1000 ~ exp(price), data = gas)

    Coefficients:
    (Intercept)   exp(price)  
           2521        -2803  

∴ Our demand function will be:

The quantity here is in thousands as you might expect from the code. The
demand function here is **limited** because we didn’t control for other
variables that affect the gas consumption, in addition to the small
sample size, only
![19](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;19 "19")
observations.

Let’s have a look then on the demand curve!

``` r
#plot the demand curve
ggplot(data=gas,mapping=aes(x=(gasq/1000),y=exp(price)))+geom_point()+geom_smooth(formula=y~x,method="lm",se=T)+scale_y_continuous(breaks=seq(0.4,0.9,by=0.1))+labs(x="Gasoline Consumption \n (in Thousands)",y="Real Gasoline Price")
```

![](project5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

After we got a hint about the demand, we can try to look into the price
elasticity of gas. We can linearly model the relationship between gas
consumption, real price, and the stock of cars. But **note that, we
can’t capture causality in that model unless we assume that the
relationship is causal**. A better way to capture causality in our case
is by using an Instrumental Variable
(![IV](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;IV "IV")).
The idea is to choose an exogenous variable that highly correlate with
the gas price but doesn’t directly affect the gas consumption. A good
example might be gas taxation, which we assume to highly correlates with
the price and then the gas consumption would be affected through the
change in the price level, but I couldn’t find a dataset for Germany
over the period
![1960-1978](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1960-1978 "1960-1978").

Let’s go with our linear multiple regression!

``` r
#summary of the model
summary(lm(gas~price+income+cars,data=gas))
```


    Call:
    lm(formula = gas ~ price + income + cars, data = gas)

    Residuals:
          Min        1Q    Median        3Q       Max 
    -0.021761 -0.010813 -0.002531  0.010642  0.033747 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  4.26353    0.27208  15.670 1.05e-10 ***
    price       -0.16706    0.06349  -2.631  0.01889 *  
    income       0.40185    0.11536   3.484  0.00333 ** 
    cars        -0.22247    0.06461  -3.443  0.00362 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.01703 on 15 degrees of freedom
    Multiple R-squared:  0.577, Adjusted R-squared:  0.4924 
    F-statistic:  6.82 on 3 and 15 DF,  p-value: 0.004043

As you can see, the price elasticity of gas is -0.17, representing the
inelastic demand. In other words, When the real gas price increases by
1%, the quantity of gas consumed per car decreases by 0.17%.
We can see also the income elasticity of gas at 0.4. When the real
income increases by 1%, the quantity of gas consumed per car increases
by 0.4%, so gasoline is a necessity good as expected.

I hope I can find a suitable instrumental variable for the gasoline
price with available data in the future.

I hope you had fun!
