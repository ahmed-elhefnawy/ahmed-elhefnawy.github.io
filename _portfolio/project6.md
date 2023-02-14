---
title: "Titanic survival"
excerpt: "The conditional probability of survival on Titanic"
collection: portfolio
---

  
I found an interesting historical data set about Titanic, the famous
British luxury passenger liner that sank in 1912.
The data is available in the package `carData` under the name
`TitanicSurvival`. You can see [this
source](https://www.britannica.com/topic/Titanic) for more historical
information about the Titanic. But for now, let’s have a closer look at
our data.

Our data has 1309 observations of 1309 passengers on four variables: Whether each passenger survived or not,
the passenger gender, the age, and the passenger class: first class,
second class, or third class.

``` r
#load the package containing the data.
library(carData)

#load the data itself.
data("TitanicSurvival")

#have a look!
TitanicSurvival[1:15,]
```

                                    survived    sex     age passengerClass
    Allen, Miss. Elisabeth Walton        yes female 29.0000            1st
    Allison, Master. Hudson Trevor       yes   male  0.9167            1st
    Allison, Miss. Helen Loraine          no female  2.0000            1st
    Allison, Mr. Hudson Joshua Crei       no   male 30.0000            1st
    Allison, Mrs. Hudson J C (Bessi       no female 25.0000            1st
    Anderson, Mr. Harry                  yes   male 48.0000            1st
    Andrews, Miss. Kornelia Theodos      yes female 63.0000            1st
    Andrews, Mr. Thomas Jr                no   male 39.0000            1st
    Appleton, Mrs. Edward Dale (Cha      yes female 53.0000            1st
    Artagaveytia, Mr. Ramon               no   male 71.0000            1st
    Astor, Col. John Jacob                no   male 47.0000            1st
    Astor, Mrs. John Jacob (Madelei      yes female 18.0000            1st
    Aubart, Mme. Leontine Pauline        yes female 24.0000            1st
    Barber, Miss. Ellen Nellie           yes female 26.0000            1st
    Barkworth, Mr. Algernon Henry W      yes   male 80.0000            1st

``` r
#create a summary for our variables.
summary(TitanicSurvival)
```

     survived      sex           age          passengerClass
     no :809   female:466   Min.   : 0.1667   1st:323       
     yes:500   male  :843   1st Qu.:21.0000   2nd:277       
                            Median :28.0000   3rd:709       
                            Mean   :29.8811                 
                            3rd Qu.:39.0000                 
                            Max.   :80.0000                 
                            NA's   :263                     

As you can see, the survivors were less than 40%, with the majority of passengers, more than 60%,
being males. The passengers’ age varied from 2 months to 80 years, but note that we don’t have the age data for 263 passengers, so we might have even younger or older passengers.
The third class passengers were more than the first class and the second
class passengers combined, and more than double the first-class
passengers.

Let’s see the survived passengers by sex and by class.

``` r
library(ggplot2)

ggplot(data=TitanicSurvival,mapping=aes(x=survived))+geom_bar(aes(fill=sex))
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-2-1.png?raw=true)<!-- -->

``` r
ggplot(data=TitanicSurvival,mapping=aes(x=survived))+geom_bar(aes(fill=passengerClass))
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-2-2.png?raw=true)<!-- -->

``` r
#passengers' gender by class.
table(TitanicSurvival$passengerClass,TitanicSurvival$sex)
```

         
          female male
      1st    144  179
      2nd    106  171
      3rd    216  493

``` r
#boxplot of the passengers' age.
boxplot(TitanicSurvival$age,horizontal = T,xlab="Passengers' Age")
abline(v=mean(TitanicSurvival$age,na.rm = T))
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-2-3.png?raw=true)<!-- -->

``` r
#passengers' age by class.
ggplot(data=TitanicSurvival,mapping=aes(x=age,fill=passengerClass))+geom_bar()
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-2-4.png?raw=true)<!-- -->

As you can tell, most of the second and third class passengers were
younger than 40.

``` r
#passengers' age by survival proportion.
ggplot(data=TitanicSurvival,mapping=aes(x=age,fill=survived))+geom_bar()
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-3-11.png?raw=true)<!-- -->

## **The Probability of survival:**

We want to see if the probability of survival differs by the class. Let
![S_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;S_i "S_i")
denotes the survival of passenger
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i"),
and
![C_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;C_i "C_i")
denotes the class of passenger
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i").
Then,

The probability of survival **conditional** on having, for example, a
first-class ticket, is the ratio between the probability that a
first-class passenger survives and the probability of a passenger
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i")
to choose a first-class ticket.

``` r
#calculate the conditional probability of survival on having a first class ticket. You can use also this code to get the same result: nrow(dplyr::filter(TitanicSurvival,survived=="yes",passengerClass=="1st"))/nrow(dplyr::filter(TitanicSurvival,passengerClass=="1st"))
mean(TitanicSurvival$survived[TitanicSurvival$passengerClass=="1st"]=="yes")
```

    [1] 0.619195

``` r
#calculate the conditional probability of survival on having a second class ticket.
mean(TitanicSurvival$survived[TitanicSurvival$passengerClass=="2nd"]=="yes")
```

    [1] 0.4296029

``` r
#calculate the conditional probability of survival on having a third class ticket.
mean(TitanicSurvival$survived[TitanicSurvival$passengerClass=="3rd"]=="yes")
```

    [1] 0.2552891

``` r
#calculate the probability of survival. You can also use this code to get the same result: nrow(dplyr::filter(TitanicSurvival,survived=="yes"))/nrow(TitanicSurvival)
mean(TitanicSurvival$survived=="yes")
```

    [1] 0.381971

The probability of survival conditional on having a first-class ticket,
a second-class ticket, and a third-class ticket is 62%, 43%, and 26%, so we can’t assume independence. Mathematically,
![\mathrm{P}(S_i \mid C_i) \not=\mathrm{P}(S_i)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathrm%7BP%7D%28S_i%20%5Cmid%20C_i%29%20%5Cnot%3D%5Cmathrm%7BP%7D%28S_i%29 "\mathrm{P}(S_i \mid C_i) \not=\mathrm{P}(S_i)").
We might also use
![\chi^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cchi%5E2 "\chi^2")
test to know the magnitude of the p-value.

``` r
#run chi-squared test.
chisq.test(TitanicSurvival$survived,TitanicSurvival$passengerClass)
```


        Pearson's Chi-squared test

    data:  TitanicSurvival$survived and TitanicSurvival$passengerClass
    X-squared = 127.86, df = 2, p-value < 2.2e-16

The p-value is almost equal to 0,
implying that the probability of survival and the ticket class are not
independent, at all levels.

To get a hint about the association between both variables, we might
calculate the Contingency Coefficient and Cramer’s V:

``` r
#first have a look on our two variables.
table(TitanicSurvival$survived,TitanicSurvival$passengerClass)
```

         
          1st 2nd 3rd
      no  123 158 528
      yes 200 119 181

Contingency Coefficient
![=\sqrt{\frac {127.86}{1309+127.86}} \approx 0.298](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%5Csqrt%7B%5Cfrac%20%7B127.86%7D%7B1309%2B127.86%7D%7D%20%5Capprox%200.298 "=\sqrt{\frac {127.86}{1309+127.86}} \approx 0.298").

Cramer’s V
![= \sqrt{\frac {127.86}{1309}} \approx 0.313](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%20%5Csqrt%7B%5Cfrac%20%7B127.86%7D%7B1309%7D%7D%20%5Capprox%200.313 "= \sqrt{\frac {127.86}{1309}} \approx 0.313").

We didn’t consider the number of rows/columns in the calculation of
Cramer’s V since the
![\min{(2-1,3-1)}=1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmin%7B%282-1%2C3-1%29%7D%3D1 "\min{(2-1,3-1)}=1").

In
![R](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R "R"),
we can calculate Contingency Coefficient and Cramer’s V as well.

``` r
library(DescTools)

#calculate Contingency Coefficient.
ContCoef(TitanicSurvival$survived,TitanicSurvival$passengerClass)
```

    [1] 0.2983038

``` r
#calculate Cramer's V.
CramerV(TitanicSurvival$survived,TitanicSurvival$passengerClass)
```

    [1] 0.3125332

The two variables are not independent, as we said before, but the
association is not very strong either.

Finally, I hope you had fun!
