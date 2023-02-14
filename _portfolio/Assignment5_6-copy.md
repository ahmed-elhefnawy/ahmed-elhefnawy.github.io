Visualization & knn regression
================

<style>
body {
text-align: justify;
line-height:2
}
</style>

  

``` r
#read and assign the data. I made sure that the data file is in the same working directory as the working directory in R.
library(data.table)
dt_cars <- fread(file = "cars.csv", sep = ";")

#having a quick look into the dataset.
head(dt_cars)
```

    ##                      VehicleName Standard SportsCar SUV Wagon Minivan Pickup
    ## 1:              Acura 3.5 RL 4dr        1         0   0     0       0      0
    ## 2: Acura 3.5 RL w/Navigation 4dr        1         0   0     0       0      0
    ## 3:                     Acura MDX        0         0   1     0       0      0
    ## 4:  Acura NSX coupe 2dr manual S        0         1   0     0       0      0
    ## 5:          Acura RSX Type S 2dr        1         0   0     0       0      0
    ## 6:                  Acura TL 4dr        1         0   0     0       0      0
    ##    AWD RWD RetailPrice Dealer Cost EngineSize Cyl  HP CityMPG HwyMPG Weight
    ## 1:   0   0       43755       39014        3.5   6 225      18     24   3880
    ## 2:   0   0       46100       41100        3.5   6 225      18     24   3893
    ## 3:   1   0       36945       33337        3.5   6 265      17     23   4451
    ## 4:   0   1       89765       79978        3.2   6 290      17     24   3153
    ## 5:   0   0       23820       21761        2.0   4 200      24     31   2778
    ## 6:   0   0       33195       30299        3.2   6 270      20     28   3575
    ##    WheelBase Length Width
    ## 1:       115    197    72
    ## 2:       115    197    72
    ## 3:       106    189    77
    ## 4:       100    174    71
    ## 5:       101    172    68
    ## 6:       108    186    72

------------------------------------------------------------------------

$\bullet$ **Create a “Drive” variable:**

``` r
#I followed the lecture's notebook to create the variable "Drive". It can be done, alternatively, using "mutate" function from "dplyr" package.

#The command first creates the Drive variable and assign FWD to all the observations, since the data doesn't have a variable for FWD, unlike AWD and RWD. Then change the Drive variable for cars that have AWD or RWD, then the command changes the Drive variable to factor variable.
dt_cars[, "Drive" := "FWD"][AWD==1, "Drive" := "AWD"][RWD==1, "Drive" := "RWD"][, "Drive" := as.factor(Drive)]
```

------------------------------------------------------------------------

$\bullet$ **The weight distribution of all cars:**

``` r
#create a histogram to show the distribution of weight. "hist" is from graphics package.
hist(dt_cars$Weight, main = "Distribution of Weight",xlab = "Weight")
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#create a boxplot for weight, which shows, from left to right, the minimum, the first quartile, the median, the third quartile, the maximum and the outliers, and the width of the box is the interquantile range.
#The default rule to set outliers is - / + 1.5 * interquantile range, from the first quartile and the second quartile, respectively.
boxplot(dt_cars$Weight,horizontal = T)
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

------------------------------------------------------------------------

$\bullet$ **The weight distribution of all cars by drive type:**

``` r
#the weight distribution by drive type. The code will generate a boxplot for each drive type.
plot(dt_cars$Drive,dt_cars$Weight,main="Weight distribution of cars by drive type",xlab="Drive",ylab="Weight")
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#the same as before, but with colors added.
plot(dt_cars$Drive,dt_cars$Weight,col=factor(dt_cars$Drive),main="Weight distribution of cars by drive type",xlab="Drive",ylab="Weight")

#loading ggplot.
library(ggplot2)
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
#using ggplot to create a histogram for the distribution of weight by drive type with a unique color for each drive type.
ggplot(dt_cars,aes(x=Weight,color=Drive))+
  geom_histogram(bins = 40)
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

------------------------------------------------------------------------

$\bullet$ **The previous plot in vertically stacked facets:**

``` r
#using ggplot.
ggplot(dt_cars,aes(x=Weight,color=Drive))+
  geom_histogram(bins = 40)+
  facet_grid(~Drive)
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

------------------------------------------------------------------------

$\bullet$ **The previous plot as a ridgeline plot:**

``` r
library(ggridges)
ggplot(data = dt_cars, aes(x=Weight, y=Drive, fill = Drive)) +
  geom_density_ridges(bandwidth=30) +
  scale_colour_brewer(type = "qual", palette = 1) #the "qual" stands for qualitative
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

------------------------------------------------------------------------

$\bullet$ **The previous plot as a ridgeline plot, density plots, with
median weights for each drive type:**

``` r
library(ggridges)
ggplot(data = dt_cars, aes(x=Weight, y=Drive,vline_color=Drive)) +
  stat_density_ridges(quantile_lines = T,quantiles = 2) #this line will calculate the median of weight for each drive type. First, the quantile lines have to be shown, so it's set to be True, and then the "quantiles" will set the number of quantiles the data should be broken into, and since the median represents the 50th percentile, then the data will be broken into two quantiles. To color each vertical line, I added "vline_color" before, in the aesthetics in ggplot, so that the vertical lines will have different colors based on the drive type.
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

------------------------------------------------------------------------

$\bullet$ **Scatterplot for horsepower and retail price, with an
emphasis on sports cars:**

``` r
#subset the dataset to only include sports cars.
sports_car<-subset(dt_cars,SportsCar==1)

#plot a scatterplot for horsepower and log of retail price, with coloring sports cars with red.
ggplot(data=dt_cars,aes(x=HP,y=log(RetailPrice)))+
  geom_point()+
  geom_point(data=sports_car, color="red")
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#the same plot as before, but in facets by drive type.
ggplot(data=dt_cars,aes(x=HP,y=log(RetailPrice)))+
  geom_point()+
  geom_point(data=sports_car, color="red")+
  facet_grid(~Drive)
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

------------------------------------------------------------------------

$\bullet$ **Plot two time series in one diagram:**

``` r
#From the assignment.
dt_timeseries <- data.table (date = seq(as.Date('2017/01/01'), as.Date('2017/12/01'), by="month"), y_1 = 1+1/(1:12),
y_2 = (1:12)^(1/3) )

#creating a step plot for the first time series.
ggplot(data=dt_timeseries,aes(x=date,y=y_1))+
  geom_step()+ 
  scale_x_date(limits = as.Date(c("2017-05-01", "2017-10-31")), date_labels = "%B",date_breaks = "1 month") #the limit for the x-axis from May 01 until October 31. The "date_labels" for the labels of the date; small b is for the initials of the month, while capital B is for the full name of the month. "date_breaks" is for the distance between breaks, which here is one month, to display all months.
```

    ## Warning: Removed 6 rows containing missing values (`geom_step()`).

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#the same plot as before, but for the second time series.
ggplot(data=dt_timeseries,aes(x=date,y=y_2))+
  geom_step()+ 
  scale_x_date(limits = as.Date(c("2017-05-01", "2017-10-31")), date_labels = "%B",date_breaks = "1 month")
```

    ## Warning: Removed 6 rows containing missing values (`geom_step()`).

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
#plotting the two time series together in one diagram.
ggplot()+
  geom_step(data=dt_timeseries,aes(x=date,y=y_1,color="First time series"))+ 
  geom_step(data=dt_timeseries,aes(x=date,y=y_2,color="Second time series"))+
  labs(x="Month",y="")+
  scale_x_date(limits = as.Date(c("2017-05-01", "2017-10-31")), date_labels = "%B",date_breaks = "1 month")  
```

    ## Warning: Removed 6 rows containing missing values (`geom_step()`).
    ## Removed 6 rows containing missing values (`geom_step()`).

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

------------------------------------------------------------------------

------------------------------------------------------------------------

$\bullet$ **One plot based on the cars dataset:**

``` r
#create a variable for the types of cars.
dt_cars[, "Type" := "Standard"][SportsCar==1, "Type" := "Sports car"][SUV==1, "Type" := "SUV"][Wagon==1, "Type":= "Wagon"][Minivan==1, "Type":= "Minivan"][Pickup==1, "Type":= "Pickup"]

#create a plot with four dimensions.
ggplot(dt_cars,aes(x=Weight,y=HP,color=Type,size=Cyl))+
  geom_point()+
  labs(y="Horse Power")
```

    ## Warning: Removed 2 rows containing missing values (`geom_point()`).

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The target is to see the relationship between weight and horsepower by
cylinders and type of car. This will be shown by the different colors
(for car’s type) and different sizes (for cylinders). (the action).
1)  Weight: Quantitative (continuous)
2)  HP: Quantitative (continuous)
3)  Cyl: Quantitative (count)
4)  Type: Qualitative (nominal)

------------------------------------------------------------------------

**Another example:**
``` r
#I will calculate a variable for the retail margin.

colnames(dt_cars)
```

    ##  [1] "VehicleName" "Standard"    "SportsCar"   "SUV"         "Wagon"      
    ##  [6] "Minivan"     "Pickup"      "AWD"         "RWD"         "RetailPrice"
    ## [11] "Dealer Cost" "EngineSize"  "Cyl"         "HP"          "CityMPG"    
    ## [16] "HwyMPG"      "Weight"      "WheelBase"   "Length"      "Width"      
    ## [21] "Drive"       "Type"

``` r
#rename the variable "Dealer cost" since the name of the variable has a space between the two words. 
colnames(dt_cars)[11]<-"DealerCost" #11 is the index of the column/variable. The number of the column.

colnames(dt_cars)
```

    ##  [1] "VehicleName" "Standard"    "SportsCar"   "SUV"         "Wagon"      
    ##  [6] "Minivan"     "Pickup"      "AWD"         "RWD"         "RetailPrice"
    ## [11] "DealerCost"  "EngineSize"  "Cyl"         "HP"          "CityMPG"    
    ## [16] "HwyMPG"      "Weight"      "WheelBase"   "Length"      "Width"      
    ## [21] "Drive"       "Type"

``` r
#create a variable for the retail margin, in percent.
dt_cars[,"margin":=((RetailPrice-DealerCost)/RetailPrice)*100]

#create a plot to show the relationship between horsepower and retail margin.
ggplot(dt_cars,aes(x=HP,y=margin,color=Type,size=EngineSize))+
  geom_point()+
  labs(x="Horse Power")
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The target is to see the relationship between horsepower and retail
margin by type of car and engine size. This will be shown by the
different colors (for car type) and different sizes (for engine size).
(the action).
1)  HP: Quantitative (continuous)
2)  margin: Quantitative (continuous)
3)  Type: Qualitative (nominal)
4)  Enginesize: Quantitative (continuous)

------------------------------------------------------------------------

------------------------------------------------------------------------

$\bullet$ **Wordcloud and radar chart:**

[Wordcloud](https://www.r-graph-gallery.com/wordcloud.html) is a visual
representation of text data. Single words and the importance of each
word is shown with font size or color. It is widely used in the media.
In `R`, there are two packages for wordcloud: `wordcloud` and
`wordcloud2`. The documentation for both can be found
[here](https://cran.r-project.org/web/packages/wordcloud/wordcloud.pdf)
and
[here](https://cran.r-project.org/web/packages/wordcloud2/wordcloud2.pdf),
respectively.
However, there are two limitations:
1)  Area is a poor metaphor of a numeric values.
2)  Longer words apperar bigger as they are composed of more letters,
    which might create a bias during the visualization.

[Radar
chart](https://www.r-graph-gallery.com/spider-or-radar-chart.html) is a
two-dimensional chart, designed to plot one or more series of values
over multiple quantitative variables. Each variable has its own axis,
all axes are joined in the center of the figure. One way to use it in
`R`, is to use `radarchart` from `fmsb` package.
However, it has some
[limitations](https://www.data-to-viz.com/caveat/spider.html) such as:
1)  The circular layout makes it harder to read, and it is difficult to
    read as well if the data is ordered.
2)  Change the order of categorical variables changes the shape
    displayed.
3)  Showing more than a couple of series would result in an unreadable
    graphic.
4)  The area of a shape in a radar chart increases quadratically rather
    than linearly, which could lead viewers to think that small changes
    are more significant than they actually are.

------------------------------------------------------------------------

------------------------------------------------------------------------

$\bullet$ **Linear models:**

``` r
#loading the data.
dt_linear<-fread(file="linear.csv",sep=";")

#having a look.
head(dt_linear)
```

    ##             x         y
    ## 1: 0.41554821 0.9390349
    ## 2: 0.64206910 0.7800644
    ## 3: 0.54541136 0.9663583
    ## 4: 0.49344160 1.0967057
    ## 5: 0.09267226 0.2850595
    ## 6: 0.51753470 1.0511716

``` r
#first linear model, which regressing on a constant only, which is the same as the mean of y.
lm(y~1,data = dt_linear)
```

    ## 
    ## Call:
    ## lm(formula = y ~ 1, data = dt_linear)
    ## 
    ## Coefficients:
    ## (Intercept)  
    ##      0.6556

``` r
#the mean of y.
mean(dt_linear$y)
```

    ## [1] 0.6556278

``` r
#second model.
lm(y~x,data=dt_linear)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = dt_linear)
    ## 
    ## Coefficients:
    ## (Intercept)            x  
    ##     0.66474     -0.01794

``` r
#third model:
lm(y~x+I(x^2),data=dt_linear)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x + I(x^2), data = dt_linear)
    ## 
    ## Coefficients:
    ## (Intercept)            x       I(x^2)  
    ##   -0.004465     3.948760    -3.795581

``` r
#plot the first model:
ggplot(dt_linear,aes(x=x,y=y))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~1,se=F) #se is for the standard error to be shown, or not, surronding the line.
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#plot the second model:
ggplot(dt_linear,aes(x=x,y=y))+
geom_point()+
  geom_smooth(method = "lm",se=F,formula = y~x)
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
#plot the third model:
ggplot(dt_linear,aes(x=x,y=y))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x+I(x^2))
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

------------------------------------------------------------------------

$\bullet$ **knn regressions:**

``` r
library(FNN)
one<-knn.reg(dt_linear,y=dt_linear$y,k=1)
one #shows "PRESS", which is the sum of squares of the predicted residuals, and "R2-Predict" is the predicted R-square. 
```

    ## PRESS =  0.04479782 
    ## R2-Predict =  0.9959699

``` r
ggplot(dt_linear,aes(x,y))+
  geom_point()+
  geom_line(aes(x=y,y=one$pred))+ #"pred" here is the predicted values given k, and here the aesthetic is different for the predicted values; as "y" is on the x-axis, and the predicted values are on the y-axis.
  labs(title = "k = 1")+
  theme(plot.title = element_text(hjust = 0.5)) #to center the plot title.
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
two<-knn.reg(dt_linear,y=dt_linear$y,k=5)
two #shows "PRESS", which is the sum of squares of the predicted residuals, and "R2-Predict" is the predicted R-square.
```

    ## PRESS =  0.09450834 
    ## R2-Predict =  0.9914978

``` r
ggplot(dt_linear,aes(x,y))+
  geom_point()+
  geom_line(aes(x=y,y=two$pred))+ #"pred" here is the predicted values given k, and here the aesthetic is different for the predicted values; as "y" is on the x-axis, and the predicted values are on the y-axis.
  labs(title = "k = 5")+
  theme(plot.title = element_text(hjust = 0.5)) #to center the plot title.
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
three<-knn.reg(dt_linear,y=dt_linear$y,k=20)
three #shows "PRESS", which is the sum of squares of the predicted residuals, and "R2-Predict" is the predicted R-square.
```

    ## PRESS =  0.9615086 
    ## R2-Predict =  0.9135003

``` r
ggplot(dt_linear,aes(x,y))+
  geom_point()+
  geom_line(aes(x=y,y=three$pred))+ #"pred" here is the predicted values given k, and here the aesthetic is different for the predicted values; as "y" is on the x-axis, and the predicted values are on the y-axis.
  labs(title = "k = 20")+
  theme(plot.title = element_text(hjust = 0.5)) #to center the plot title.
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

``` r
four<-knn.reg(dt_linear,y=dt_linear$y,k=109)
four #shows "PRESS", which is the sum of squares of the predicted residuals, and "R2-Predict" is the predicted R-square.
```

    ## PRESS =  11.32064 
    ## R2-Predict =  -0.01843279

``` r
ggplot(dt_linear,aes(x,y))+
  geom_point()+
  geom_line(aes(x=y,y=four$pred))+ #"pred" here is the predicted values given k, and here the aesthetic is different for the predicted values; as "y" is on the x-axis, and the predicted values are on the y-axis.
  labs(title = "k = 109")+
  theme(plot.title = element_text(hjust = 0.5)) #to center the plot title.
```

![](Assignment5_6-copy_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->
