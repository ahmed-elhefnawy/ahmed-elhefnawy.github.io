---
title: "Change of CO<sub>2</sub> emissions per capita across countries in the long run"
excerpt: "Short description"
collection: portfolio
---
  

I wanted to see if the change of carbon dioxide emissions per capita,
measured in metric tons, is significant in the long run. We expect it to
be positive and distinguishable from zero. The dataset is
[here](https://data.worldbank.org/indicator/EN.ATM.CO2E.PC?view=chart)

First, we might calculate the change rate of the emissions for each
country over the period 1960-2016.

``` r
#load the libraries we need.
library(readxl)
library(dplyr)
library(data.table)
library(EnvStats)
library(DT)

#import the dataset. I renamed the file as you can see.
emission<-read_xls("carbondioxideemmision.xls")

#now let's see the first small part of the data
emission[1:7,1:7] #alternatively, you can use head() function to review the first parts of the data.
```

    # A tibble: 7 × 7
      `Data Source`     `World Development Indicators` ...3  ...4  ...5  ...6  ...7 
      <chr>             <chr>                          <chr> <chr> <chr> <chr> <chr>
    1 Last Updated Date 44274                          <NA>  <NA>  <NA>  <NA>  <NA> 
    2 <NA>              <NA>                           <NA>  <NA>  <NA>  <NA>  <NA> 
    3 Country Name      Country Code                   Indi… Indi… 1960  1961  1962 
    4 Aruba             ABW                            CO2 … EN.A… 204.… 208.… 226.…
    5 Afghanistan       AFG                            CO2 … EN.A… 0.04… 0.05… 0.07…
    6 Angola            AGO                            CO2 … EN.A… 0.10… 0.08… 0.21…
    7 Albania           ALB                            CO2 … EN.A… 1.25… 1.37… 1.43…

``` r
#remove the first two rows, since they contain nothing.
emission<-emission[-c(1:2),]

#the first row contains the names of columns, yet the columns are not named properly. Change the columns names to be as the first row
colnames(emission)<-emission[1,]

#after we renamed the columns by the first row, we don't need the first row anymore, so let's delete it.
emission<-emission[-c(1),]

#Now let's see again:

emission[1:5,]
```

    # A tibble: 5 × 65
      Country Na…¹ Count…² Indic…³ Indic…⁴ `1960` `1961` `1962` `1963` `1964` `1965`
      <chr>        <chr>   <chr>   <chr>   <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    1 Aruba        ABW     CO2 em… EN.ATM… 204.6… 208.8… 226.1… 214.8… 207.6… 185.2…
    2 Afghanistan  AFG     CO2 em… EN.ATM… 0.046… 0.053… 0.073… 0.074… 0.086… 0.101…
    3 Angola       AGO     CO2 em… EN.ATM… 0.100… 0.082… 0.210… 0.202… 0.213… 0.205…
    4 Albania      ALB     CO2 em… EN.ATM… 1.258… 1.374… 1.439… 1.181… 1.111… 1.166…
    5 Andorra      AND     CO2 em… EN.ATM… <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    # … with 55 more variables: `1966` <chr>, `1967` <chr>, `1968` <chr>,
    #   `1969` <chr>, `1970` <chr>, `1971` <chr>, `1972` <chr>, `1973` <chr>,
    #   `1974` <chr>, `1975` <chr>, `1976` <chr>, `1977` <chr>, `1978` <chr>,
    #   `1979` <chr>, `1980` <chr>, `1981` <chr>, `1982` <chr>, `1983` <chr>,
    #   `1984` <chr>, `1985` <chr>, `1986` <chr>, `1987` <chr>, `1988` <chr>,
    #   `1989` <chr>, `1990` <chr>, `1991` <chr>, `1992` <chr>, `1993` <chr>,
    #   `1994` <chr>, `1995` <chr>, `1996` <chr>, `1997` <chr>, `1998` <chr>, …

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

                      Aruba          Afghanistan               Angola
    1960 204.62037224917452 0.046056712629903414   0.1008353356494021
    1961 208.82281106822035 0.053588835050455808 0.082203796747050334
    1962 226.11807914628724 0.073720830832381873  0.21053147709234077
    1963 214.80037040303375 0.074160724829865854  0.20273730345395635
    1964 207.61577710758871 0.086173614368552767  0.21356034931902876
    1965 185.20395746164576  0.10128491249779034  0.20589092585307864
    1966 172.11995148574894  0.10739888092545179  0.26894143686775823
                    Albania Andorra          Arab World United Arab Emirates
    1960 1.2581949278965689    <NA> 0.60744755675365358  0.11903525287281698
    1961 1.3741860465116278    <NA> 0.66063794460729808  0.10914123576332393
    1962 1.4399559637916719    <NA> 0.72494431853622876  0.16353306337965359
    1963 1.1816811441597486    <NA> 0.85056679990052364  0.17583313354111724
    1964 1.1117419596667282    <NA> 0.96947620360177611  0.13282478140235732
    1965 1.1660990427345477    <NA>  1.1352713004209469  0.14681996836984593
    1966 1.3330554645866206    <NA>  1.2482565638723149  0.16045531829774465

``` r
#now before playing with these numbers let's see if R recognize them as numbers. Let's take the first country as an example.
class(emission$Aruba)
```

    [1] "character"

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

       year    Aruba Afghanistan    Angola  Albania
    1: 1960 204.6204  0.04605671 0.1008353 1.258195
    2: 1961 208.8228  0.05358884 0.0822038 1.374186
    3: 1962 226.1181  0.07372083 0.2105315 1.439956
    4: 1963 214.8004  0.07416072 0.2027373 1.181681
    5: 1964 207.6158  0.08617361 0.2135603 1.111742

``` r
#let's transform our data to long format so that we'd have the countries as a column altogether, resulting in a panel data.
emission<-melt(emission, id.vars="year",variable.name = "country")

head(emission)
```

       year country    value
    1: 1960   Aruba 204.6204
    2: 1961   Aruba 208.8228
    3: 1962   Aruba 226.1181
    4: 1963   Aruba 214.8004
    5: 1964   Aruba 207.6158
    6: 1965   Aruba 185.2040

``` r
#filter our data so that we would have only the observations for 1960 and 2016.
emission_6016<-dplyr::filter(emission,year %in% c(1960,2016))

#let's calculate the change rate.
emission_6016<- emission_6016 %>%
    group_by(country) %>%
    mutate(change=(((value/lag(value))^(1/(2016-1960)))-1)*100)

#let's have a look.
head(emission_6016)
```

    # A tibble: 6 × 4
    # Groups:   country [3]
      year  country        value change
      <chr> <fct>          <dbl>  <dbl>
    1 1960  Aruba       205.      NA   
    2 2016  Aruba         8.43    -5.54
    3 1960  Afghanistan   0.0461  NA   
    4 2016  Afghanistan   0.245    3.03
    5 1960  Angola        0.101   NA   
    6 2016  Angola        1.20     4.53

``` r
#we only need the "country" and its change rate, so we can remove the other two columns.
emission_6016<-emission_6016[,-c(1,3)]

#remove the missing values, since it's only one value for each country.
emission_6016<-na.omit(emission_6016)

#let's have another look.
head(emission_6016)
```

    # A tibble: 6 × 2
    # Groups:   country [6]
      country              change
      <fct>                 <dbl>
    1 Aruba                -5.54 
    2 Afghanistan           3.03 
    3 Angola                4.53 
    4 Albania               0.404
    5 Arab World            3.73 
    6 United Arab Emirates  9.77 

In addition, let’s consider the average emissions per capita for each
country over the same period, and the coefficient of variation as well.

``` r
#turning back to our original data, calculate the mean and the coefficient of variation by country and then merge it with the subset with the change rate that we created it before.
emission<-inner_join(emission_6016,emission %>%
             group_by(country) %>%
             summarise(mean=mean(value,na.rm = T), cv=cv(value,na.rm=T)))
```

    Joining, by = "country"

``` r
#round our variables to the fourth digit
emission$change<-round(emission$change,4)
emission$mean<-round(emission$mean,4)
emission$cv<-round(emission$cv,4)

head(emission)
```

    # A tibble: 6 × 4
    # Groups:   country [6]
      country              change    mean    cv
      <fct>                 <dbl>   <dbl> <dbl>
    1 Aruba                -5.54  105.    0.996
    2 Afghanistan           3.03    0.148 0.603
    3 Angola                4.53    0.652 0.558
    4 Albania               0.404   1.65  0.393
    5 Arab World            3.73    3.03  0.396
    6 United Arab Emirates  9.77   31.0   0.681

``` r
#since the world bank includes regions among countries, I created a vector manually to contain all regions that are not individual countries, since including them might bias our calculations.
regions<-c("Arab World","Central Europe and the Baltics","Caribbean small states","East Asia & Pacific (excluding high income)","Early-demographic dividend","East Asia & Pacific","Europe & Central Asia (excluding high income)","Europe & Central Asia","European Union","Fragile and conflict affected situations","High income","Heavily indebted poor countries (HIPC)","IBRD only", "IDA & IBRD total","IDA total","IDA blend","IDA only","Latin America & Caribbean (excluding high income)","Latin America & Caribbean","Least developed countries: UN classification","Low income","Lower middle income","Low & middle income","Late-demographic dividend","Middle East & North Africa","Middle income","Middle East & North Africa (excluding high income)","North America","OECD members","Pre-demographic dividend","Pacific island small states","Other small states","Post-demographic dividend","South Asia","Sub-Saharan Africa (excluding high income)","Sub-Saharan Africa","Small states","East Asia & Pacific (IDA & IBRD countries)","Europe & Central Asia (IDA & IBRD countries)","Latin America & the Caribbean (IDA & IBRD countries)","Middle East & North Africa (IDA & IBRD countries)","South Asia (IDA & IBRD)","Sub-Saharan Africa (IDA & IBRD countries)","Upper middle income","World")

#quickly create a new subset to include these regions only, and then filter our data to include only the places that are not in the subset created. there are many ways to do that, I just chose what came to my mind. 
emission2<-filter(emission,country%in%regions)
emission<-emission[!(emission$country %in% emission2$country),]

#let's create a table to show our data.
knitr::kable(emission, format = "html", col.names = c("country","percentage change of emissions per capita \n (1960-2016)","Average of emissions per capita (in metric tons) \n (1960-2016)","coefficent of variation"))
```

<table>
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:right;">
percentage change of emissions per capita (1960-2016)
</th>
<th style="text-align:right;">
Average of emissions per capita (in metric tons) (1960-2016)
</th>
<th style="text-align:right;">
coefficent of variation
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Aruba
</td>
<td style="text-align:right;">
-5.5368
</td>
<td style="text-align:right;">
104.7306
</td>
<td style="text-align:right;">
0.9957
</td>
</tr>
<tr>
<td style="text-align:left;">
Afghanistan
</td>
<td style="text-align:right;">
3.0304
</td>
<td style="text-align:right;">
0.1480
</td>
<td style="text-align:right;">
0.6027
</td>
</tr>
<tr>
<td style="text-align:left;">
Angola
</td>
<td style="text-align:right;">
4.5262
</td>
<td style="text-align:right;">
0.6517
</td>
<td style="text-align:right;">
0.5584
</td>
</tr>
<tr>
<td style="text-align:left;">
Albania
</td>
<td style="text-align:right;">
0.4043
</td>
<td style="text-align:right;">
1.6517
</td>
<td style="text-align:right;">
0.3928
</td>
</tr>
<tr>
<td style="text-align:left;">
United Arab Emirates
</td>
<td style="text-align:right;">
9.7721
</td>
<td style="text-align:right;">
31.0151
</td>
<td style="text-align:right;">
0.6809
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
1.1886
</td>
<td style="text-align:right;">
3.6885
</td>
<td style="text-align:right;">
0.1667
</td>
</tr>
<tr>
<td style="text-align:left;">
Antigua and Barbuda
</td>
<td style="text-align:right;">
3.9396
</td>
<td style="text-align:right;">
5.2642
</td>
<td style="text-align:right;">
0.5957
</td>
</tr>
<tr>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:right;">
1.0656
</td>
<td style="text-align:right;">
14.5646
</td>
<td style="text-align:right;">
0.1859
</td>
</tr>
<tr>
<td style="text-align:left;">
Austria
</td>
<td style="text-align:right;">
0.8521
</td>
<td style="text-align:right;">
7.1936
</td>
<td style="text-align:right;">
0.1503
</td>
</tr>
<tr>
<td style="text-align:left;">
Belgium
</td>
<td style="text-align:right;">
-0.2688
</td>
<td style="text-align:right;">
11.1275
</td>
<td style="text-align:right;">
0.1328
</td>
</tr>
<tr>
<td style="text-align:left;">
Benin
</td>
<td style="text-align:right;">
3.9968
</td>
<td style="text-align:right;">
0.2176
</td>
<td style="text-align:right;">
0.7736
</td>
</tr>
<tr>
<td style="text-align:left;">
Burkina Faso
</td>
<td style="text-align:right;">
5.5060
</td>
<td style="text-align:right;">
0.0714
</td>
<td style="text-align:right;">
0.6308
</td>
</tr>
<tr>
<td style="text-align:left;">
Bangladesh
</td>
<td style="text-align:right;">
1.0641
</td>
<td style="text-align:right;">
0.2413
</td>
<td style="text-align:right;">
0.5605
</td>
</tr>
<tr>
<td style="text-align:left;">
Bulgaria
</td>
<td style="text-align:right;">
1.3031
</td>
<td style="text-align:right;">
7.0697
</td>
<td style="text-align:right;">
0.2531
</td>
</tr>
<tr>
<td style="text-align:left;">
Bahrain
</td>
<td style="text-align:right;">
3.3329
</td>
<td style="text-align:right;">
20.4557
</td>
<td style="text-align:right;">
0.3546
</td>
</tr>
<tr>
<td style="text-align:left;">
Bahamas, The
</td>
<td style="text-align:right;">
0.4139
</td>
<td style="text-align:right;">
12.6245
</td>
<td style="text-align:right;">
0.9974
</td>
</tr>
<tr>
<td style="text-align:left;">
Belize
</td>
<td style="text-align:right;">
2.1146
</td>
<td style="text-align:right;">
1.3571
</td>
<td style="text-align:right;">
0.2748
</td>
</tr>
<tr>
<td style="text-align:left;">
Bermuda
</td>
<td style="text-align:right;">
1.7700
</td>
<td style="text-align:right;">
7.5946
</td>
<td style="text-align:right;">
0.3053
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia
</td>
<td style="text-align:right;">
3.5695
</td>
<td style="text-align:right;">
0.9748
</td>
<td style="text-align:right;">
0.4629
</td>
</tr>
<tr>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:right;">
2.2362
</td>
<td style="text-align:right;">
1.5104
</td>
<td style="text-align:right;">
0.3317
</td>
</tr>
<tr>
<td style="text-align:left;">
Barbados
</td>
<td style="text-align:right;">
3.2464
</td>
<td style="text-align:right;">
3.2366
</td>
<td style="text-align:right;">
0.4399
</td>
</tr>
<tr>
<td style="text-align:left;">
Brunei Darussalam
</td>
<td style="text-align:right;">
2.7099
</td>
<td style="text-align:right;">
20.9130
</td>
<td style="text-align:right;">
0.7876
</td>
</tr>
<tr>
<td style="text-align:left;">
Central African Republic
</td>
<td style="text-align:right;">
0.1976
</td>
<td style="text-align:right;">
0.0677
</td>
<td style="text-align:right;">
0.2268
</td>
</tr>
<tr>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
0.6039
</td>
<td style="text-align:right;">
15.7752
</td>
<td style="text-align:right;">
0.1164
</td>
</tr>
<tr>
<td style="text-align:left;">
Switzerland
</td>
<td style="text-align:right;">
0.2084
</td>
<td style="text-align:right;">
5.6816
</td>
<td style="text-align:right;">
0.1387
</td>
</tr>
<tr>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
1.8823
</td>
<td style="text-align:right;">
2.8843
</td>
<td style="text-align:right;">
0.3427
</td>
</tr>
<tr>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
3.2912
</td>
<td style="text-align:right;">
2.7341
</td>
<td style="text-align:right;">
0.7767
</td>
</tr>
<tr>
<td style="text-align:left;">
Cote d’Ivoire
</td>
<td style="text-align:right;">
2.0286
</td>
<td style="text-align:right;">
0.4556
</td>
<td style="text-align:right;">
0.3550
</td>
</tr>
<tr>
<td style="text-align:left;">
Cameroon
</td>
<td style="text-align:right;">
3.4302
</td>
<td style="text-align:right;">
0.2387
</td>
<td style="text-align:right;">
0.7174
</td>
</tr>
<tr>
<td style="text-align:left;">
Congo, Dem. Rep. 
</td>
<td style="text-align:right;">
-3.1304
</td>
<td style="text-align:right;">
0.0931
</td>
<td style="text-align:right;">
0.5691
</td>
</tr>
<tr>
<td style="text-align:left;">
Congo, Rep. 
</td>
<td style="text-align:right;">
1.9808
</td>
<td style="text-align:right;">
0.4640
</td>
<td style="text-align:right;">
0.4634
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:right;">
1.2335
</td>
<td style="text-align:right;">
1.5424
</td>
<td style="text-align:right;">
0.1478
</td>
</tr>
<tr>
<td style="text-align:left;">
Comoros
</td>
<td style="text-align:right;">
2.6827
</td>
<td style="text-align:right;">
0.1476
</td>
<td style="text-align:right;">
0.3603
</td>
</tr>
<tr>
<td style="text-align:left;">
Cabo Verde
</td>
<td style="text-align:right;">
4.0765
</td>
<td style="text-align:right;">
0.4463
</td>
<td style="text-align:right;">
0.7892
</td>
</tr>
<tr>
<td style="text-align:left;">
Costa Rica
</td>
<td style="text-align:right;">
2.6956
</td>
<td style="text-align:right;">
1.1229
</td>
<td style="text-align:right;">
0.3784
</td>
</tr>
<tr>
<td style="text-align:left;">
Cuba
</td>
<td style="text-align:right;">
0.4705
</td>
<td style="text-align:right;">
2.5115
</td>
<td style="text-align:right;">
0.2044
</td>
</tr>
<tr>
<td style="text-align:left;">
Cayman Islands
</td>
<td style="text-align:right;">
3.3246
</td>
<td style="text-align:right;">
7.8378
</td>
<td style="text-align:right;">
0.4392
</td>
</tr>
<tr>
<td style="text-align:left;">
Cyprus
</td>
<td style="text-align:right;">
2.3419
</td>
<td style="text-align:right;">
5.0497
</td>
<td style="text-align:right;">
0.3909
</td>
</tr>
<tr>
<td style="text-align:left;">
Djibouti
</td>
<td style="text-align:right;">
0.5807
</td>
<td style="text-align:right;">
0.6923
</td>
<td style="text-align:right;">
0.2644
</td>
</tr>
<tr>
<td style="text-align:left;">
Dominica
</td>
<td style="text-align:right;">
4.7911
</td>
<td style="text-align:right;">
1.0261
</td>
<td style="text-align:right;">
0.7757
</td>
</tr>
<tr>
<td style="text-align:left;">
Denmark
</td>
<td style="text-align:right;">
-0.2828
</td>
<td style="text-align:right;">
9.9830
</td>
<td style="text-align:right;">
0.1931
</td>
</tr>
<tr>
<td style="text-align:left;">
Dominican Republic
</td>
<td style="text-align:right;">
3.7084
</td>
<td style="text-align:right;">
1.4625
</td>
<td style="text-align:right;">
0.4745
</td>
</tr>
<tr>
<td style="text-align:left;">
Algeria
</td>
<td style="text-align:right;">
3.4383
</td>
<td style="text-align:right;">
2.5084
</td>
<td style="text-align:right;">
0.4244
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador
</td>
<td style="text-align:right;">
3.3786
</td>
<td style="text-align:right;">
1.5853
</td>
<td style="text-align:right;">
0.4666
</td>
</tr>
<tr>
<td style="text-align:left;">
Egypt, Arab Rep. 
</td>
<td style="text-align:right;">
2.5915
</td>
<td style="text-align:right;">
1.4477
</td>
<td style="text-align:right;">
0.4560
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:right;">
2.1367
</td>
<td style="text-align:right;">
5.2077
</td>
<td style="text-align:right;">
0.3283
</td>
</tr>
<tr>
<td style="text-align:left;">
Ethiopia
</td>
<td style="text-align:right;">
4.0080
</td>
<td style="text-align:right;">
0.0574
</td>
<td style="text-align:right;">
0.4595
</td>
</tr>
<tr>
<td style="text-align:left;">
Finland
</td>
<td style="text-align:right;">
1.6115
</td>
<td style="text-align:right;">
9.5432
</td>
<td style="text-align:right;">
0.2471
</td>
</tr>
<tr>
<td style="text-align:left;">
Fiji
</td>
<td style="text-align:right;">
2.8209
</td>
<td style="text-align:right;">
1.0979
</td>
<td style="text-align:right;">
0.3544
</td>
</tr>
<tr>
<td style="text-align:left;">
Faroe Islands
</td>
<td style="text-align:right;">
3.7182
</td>
<td style="text-align:right;">
10.4409
</td>
<td style="text-align:right;">
0.3802
</td>
</tr>
<tr>
<td style="text-align:left;">
Gabon
</td>
<td style="text-align:right;">
4.2078
</td>
<td style="text-align:right;">
4.4558
</td>
<td style="text-align:right;">
0.6093
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:right;">
-1.1675
</td>
<td style="text-align:right;">
9.7055
</td>
<td style="text-align:right;">
0.1516
</td>
</tr>
<tr>
<td style="text-align:left;">
Ghana
</td>
<td style="text-align:right;">
1.7584
</td>
<td style="text-align:right;">
0.3101
</td>
<td style="text-align:right;">
0.3348
</td>
</tr>
<tr>
<td style="text-align:left;">
Gibraltar
</td>
<td style="text-align:right;">
4.0501
</td>
<td style="text-align:right;">
6.6509
</td>
<td style="text-align:right;">
0.7819
</td>
</tr>
<tr>
<td style="text-align:left;">
Guinea
</td>
<td style="text-align:right;">
1.3942
</td>
<td style="text-align:right;">
0.1939
</td>
<td style="text-align:right;">
0.1322
</td>
</tr>
<tr>
<td style="text-align:left;">
Gambia, The
</td>
<td style="text-align:right;">
2.8882
</td>
<td style="text-align:right;">
0.1829
</td>
<td style="text-align:right;">
0.3307
</td>
</tr>
<tr>
<td style="text-align:left;">
Guinea-Bissau
</td>
<td style="text-align:right;">
3.1013
</td>
<td style="text-align:right;">
0.1454
</td>
<td style="text-align:right;">
0.3002
</td>
</tr>
<tr>
<td style="text-align:left;">
Equatorial Guinea
</td>
<td style="text-align:right;">
7.3830
</td>
<td style="text-align:right;">
2.4384
</td>
<td style="text-align:right;">
1.5230
</td>
</tr>
<tr>
<td style="text-align:left;">
Greece
</td>
<td style="text-align:right;">
2.9637
</td>
<td style="text-align:right;">
5.7733
</td>
<td style="text-align:right;">
0.4145
</td>
</tr>
<tr>
<td style="text-align:left;">
Grenada
</td>
<td style="text-align:right;">
4.1832
</td>
<td style="text-align:right;">
1.1918
</td>
<td style="text-align:right;">
0.6775
</td>
</tr>
<tr>
<td style="text-align:left;">
Greenland
</td>
<td style="text-align:right;">
0.4944
</td>
<td style="text-align:right;">
8.8976
</td>
<td style="text-align:right;">
0.2525
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala
</td>
<td style="text-align:right;">
2.1281
</td>
<td style="text-align:right;">
0.6261
</td>
<td style="text-align:right;">
0.3285
</td>
</tr>
<tr>
<td style="text-align:left;">
Guyana
</td>
<td style="text-align:right;">
1.7739
</td>
<td style="text-align:right;">
1.9819
</td>
<td style="text-align:right;">
0.2245
</td>
</tr>
<tr>
<td style="text-align:left;">
Hong Kong SAR, China
</td>
<td style="text-align:right;">
3.3089
</td>
<td style="text-align:right;">
4.2088
</td>
<td style="text-align:right;">
0.4389
</td>
</tr>
<tr>
<td style="text-align:left;">
Honduras
</td>
<td style="text-align:right;">
2.2637
</td>
<td style="text-align:right;">
0.6510
</td>
<td style="text-align:right;">
0.3910
</td>
</tr>
<tr>
<td style="text-align:left;">
Haiti
</td>
<td style="text-align:right;">
2.3702
</td>
<td style="text-align:right;">
0.1444
</td>
<td style="text-align:right;">
0.4380
</td>
</tr>
<tr>
<td style="text-align:left;">
Hungary
</td>
<td style="text-align:right;">
0.0371
</td>
<td style="text-align:right;">
6.2494
</td>
<td style="text-align:right;">
0.1908
</td>
</tr>
<tr>
<td style="text-align:left;">
Indonesia
</td>
<td style="text-align:right;">
3.9661
</td>
<td style="text-align:right;">
0.9430
</td>
<td style="text-align:right;">
0.6143
</td>
</tr>
<tr>
<td style="text-align:left;">
India
</td>
<td style="text-align:right;">
3.4802
</td>
<td style="text-align:right;">
0.7597
</td>
<td style="text-align:right;">
0.5755
</td>
</tr>
<tr>
<td style="text-align:left;">
Ireland
</td>
<td style="text-align:right;">
1.2511
</td>
<td style="text-align:right;">
7.9648
</td>
<td style="text-align:right;">
0.2305
</td>
</tr>
<tr>
<td style="text-align:left;">
Iran, Islamic Rep. 
</td>
<td style="text-align:right;">
2.8682
</td>
<td style="text-align:right;">
4.6137
</td>
<td style="text-align:right;">
0.4323
</td>
</tr>
<tr>
<td style="text-align:left;">
Iraq
</td>
<td style="text-align:right;">
2.7540
</td>
<td style="text-align:right;">
3.1510
</td>
<td style="text-align:right;">
0.3120
</td>
</tr>
<tr>
<td style="text-align:left;">
Iceland
</td>
<td style="text-align:right;">
-0.2073
</td>
<td style="text-align:right;">
7.2322
</td>
<td style="text-align:right;">
0.0998
</td>
</tr>
<tr>
<td style="text-align:left;">
Israel
</td>
<td style="text-align:right;">
1.6439
</td>
<td style="text-align:right;">
6.9423
</td>
<td style="text-align:right;">
0.2820
</td>
</tr>
<tr>
<td style="text-align:left;">
Jamaica
</td>
<td style="text-align:right;">
2.0610
</td>
<td style="text-align:right;">
2.9976
</td>
<td style="text-align:right;">
0.2853
</td>
</tr>
<tr>
<td style="text-align:left;">
Jordan
</td>
<td style="text-align:right;">
2.1522
</td>
<td style="text-align:right;">
2.3331
</td>
<td style="text-align:right;">
0.3937
</td>
</tr>
<tr>
<td style="text-align:left;">
Japan
</td>
<td style="text-align:right;">
2.2904
</td>
<td style="text-align:right;">
7.9160
</td>
<td style="text-align:right;">
0.2535
</td>
</tr>
<tr>
<td style="text-align:left;">
Kenya
</td>
<td style="text-align:right;">
0.3576
</td>
<td style="text-align:right;">
0.2842
</td>
<td style="text-align:right;">
0.1762
</td>
</tr>
<tr>
<td style="text-align:left;">
Cambodia
</td>
<td style="text-align:right;">
4.9967
</td>
<td style="text-align:right;">
0.1447
</td>
<td style="text-align:right;">
0.9601
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Kitts and Nevis
</td>
<td style="text-align:right;">
5.6303
</td>
<td style="text-align:right;">
2.2936
</td>
<td style="text-align:right;">
0.7181
</td>
</tr>
<tr>
<td style="text-align:left;">
Korea, Rep. 
</td>
<td style="text-align:right;">
5.8497
</td>
<td style="text-align:right;">
5.8387
</td>
<td style="text-align:right;">
0.6661
</td>
</tr>
<tr>
<td style="text-align:left;">
Kuwait
</td>
<td style="text-align:right;">
-0.2684
</td>
<td style="text-align:right;">
28.1521
</td>
<td style="text-align:right;">
0.4530
</td>
</tr>
<tr>
<td style="text-align:left;">
Lao PDR
</td>
<td style="text-align:right;">
7.8320
</td>
<td style="text-align:right;">
0.2363
</td>
<td style="text-align:right;">
1.6330
</td>
</tr>
<tr>
<td style="text-align:left;">
Lebanon
</td>
<td style="text-align:right;">
1.7092
</td>
<td style="text-align:right;">
2.9562
</td>
<td style="text-align:right;">
0.3086
</td>
</tr>
<tr>
<td style="text-align:left;">
Liberia
</td>
<td style="text-align:right;">
1.2889
</td>
<td style="text-align:right;">
0.4251
</td>
<td style="text-align:right;">
0.7731
</td>
</tr>
<tr>
<td style="text-align:left;">
Libya
</td>
<td style="text-align:right;">
5.1079
</td>
<td style="text-align:right;">
7.7510
</td>
<td style="text-align:right;">
0.4346
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Lucia
</td>
<td style="text-align:right;">
4.8355
</td>
<td style="text-align:right;">
1.3324
</td>
<td style="text-align:right;">
0.5781
</td>
</tr>
<tr>
<td style="text-align:left;">
Sri Lanka
</td>
<td style="text-align:right;">
2.8471
</td>
<td style="text-align:right;">
0.3959
</td>
<td style="text-align:right;">
0.5561
</td>
</tr>
<tr>
<td style="text-align:left;">
Luxembourg
</td>
<td style="text-align:right;">
-1.5332
</td>
<td style="text-align:right;">
27.2603
</td>
<td style="text-align:right;">
0.2720
</td>
</tr>
<tr>
<td style="text-align:left;">
Macao SAR, China
</td>
<td style="text-align:right;">
4.3800
</td>
<td style="text-align:right;">
2.2823
</td>
<td style="text-align:right;">
0.5034
</td>
</tr>
<tr>
<td style="text-align:left;">
Morocco
</td>
<td style="text-align:right;">
3.2223
</td>
<td style="text-align:right;">
0.9646
</td>
<td style="text-align:right;">
0.4955
</td>
</tr>
<tr>
<td style="text-align:left;">
Madagascar
</td>
<td style="text-align:right;">
1.2467
</td>
<td style="text-align:right;">
0.1132
</td>
<td style="text-align:right;">
0.2823
</td>
</tr>
<tr>
<td style="text-align:left;">
Mexico
</td>
<td style="text-align:right;">
1.5453
</td>
<td style="text-align:right;">
3.4244
</td>
<td style="text-align:right;">
0.2733
</td>
</tr>
<tr>
<td style="text-align:left;">
Mali
</td>
<td style="text-align:right;">
3.7117
</td>
<td style="text-align:right;">
0.0671
</td>
<td style="text-align:right;">
0.5997
</td>
</tr>
<tr>
<td style="text-align:left;">
Malta
</td>
<td style="text-align:right;">
1.8700
</td>
<td style="text-align:right;">
4.2710
</td>
<td style="text-align:right;">
0.4617
</td>
</tr>
<tr>
<td style="text-align:left;">
Myanmar
</td>
<td style="text-align:right;">
2.4186
</td>
<td style="text-align:right;">
0.1793
</td>
<td style="text-align:right;">
0.4061
</td>
</tr>
<tr>
<td style="text-align:left;">
Mongolia
</td>
<td style="text-align:right;">
3.2899
</td>
<td style="text-align:right;">
4.1000
</td>
<td style="text-align:right;">
0.6209
</td>
</tr>
<tr>
<td style="text-align:left;">
Mozambique
</td>
<td style="text-align:right;">
0.1506
</td>
<td style="text-align:right;">
0.1800
</td>
<td style="text-align:right;">
0.5763
</td>
</tr>
<tr>
<td style="text-align:left;">
Mauritania
</td>
<td style="text-align:right;">
4.9865
</td>
<td style="text-align:right;">
0.4846
</td>
<td style="text-align:right;">
0.6599
</td>
</tr>
<tr>
<td style="text-align:left;">
Mauritius
</td>
<td style="text-align:right;">
4.6330
</td>
<td style="text-align:right;">
1.4815
</td>
<td style="text-align:right;">
0.7057
</td>
</tr>
<tr>
<td style="text-align:left;">
Malaysia
</td>
<td style="text-align:right;">
5.3481
</td>
<td style="text-align:right;">
3.7809
</td>
<td style="text-align:right;">
0.6885
</td>
</tr>
<tr>
<td style="text-align:left;">
New Caledonia
</td>
<td style="text-align:right;">
1.0210
</td>
<td style="text-align:right;">
12.5874
</td>
<td style="text-align:right;">
0.3056
</td>
</tr>
<tr>
<td style="text-align:left;">
Niger
</td>
<td style="text-align:right;">
4.4097
</td>
<td style="text-align:right;">
0.0711
</td>
<td style="text-align:right;">
0.4863
</td>
</tr>
<tr>
<td style="text-align:left;">
Nigeria
</td>
<td style="text-align:right;">
3.9122
</td>
<td style="text-align:right;">
0.5706
</td>
<td style="text-align:right;">
0.4541
</td>
</tr>
<tr>
<td style="text-align:left;">
Nicaragua
</td>
<td style="text-align:right;">
1.9556
</td>
<td style="text-align:right;">
0.6396
</td>
<td style="text-align:right;">
0.2430
</td>
</tr>
<tr>
<td style="text-align:left;">
Netherlands
</td>
<td style="text-align:right;">
0.8054
</td>
<td style="text-align:right;">
10.4779
</td>
<td style="text-align:right;">
0.1408
</td>
</tr>
<tr>
<td style="text-align:left;">
Norway
</td>
<td style="text-align:right;">
1.3696
</td>
<td style="text-align:right;">
8.1035
</td>
<td style="text-align:right;">
0.2490
</td>
</tr>
<tr>
<td style="text-align:left;">
Nepal
</td>
<td style="text-align:right;">
6.8947
</td>
<td style="text-align:right;">
0.0811
</td>
<td style="text-align:right;">
0.9419
</td>
</tr>
<tr>
<td style="text-align:left;">
New Zealand
</td>
<td style="text-align:right;">
0.7329
</td>
<td style="text-align:right;">
6.7170
</td>
<td style="text-align:right;">
0.1875
</td>
</tr>
<tr>
<td style="text-align:left;">
Pakistan
</td>
<td style="text-align:right;">
2.0641
</td>
<td style="text-align:right;">
0.5982
</td>
<td style="text-align:right;">
0.3733
</td>
</tr>
<tr>
<td style="text-align:left;">
Panama
</td>
<td style="text-align:right;">
1.9902
</td>
<td style="text-align:right;">
1.7285
</td>
<td style="text-align:right;">
0.3128
</td>
</tr>
<tr>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:right;">
1.5036
</td>
<td style="text-align:right;">
1.2275
</td>
<td style="text-align:right;">
0.2305
</td>
</tr>
<tr>
<td style="text-align:left;">
Philippines
</td>
<td style="text-align:right;">
2.3726
</td>
<td style="text-align:right;">
0.7503
</td>
<td style="text-align:right;">
0.2540
</td>
</tr>
<tr>
<td style="text-align:left;">
Palau
</td>
<td style="text-align:right;">
3.8750
</td>
<td style="text-align:right;">
11.0785
</td>
<td style="text-align:right;">
0.2685
</td>
</tr>
<tr>
<td style="text-align:left;">
Papua New Guinea
</td>
<td style="text-align:right;">
4.4477
</td>
<td style="text-align:right;">
0.4751
</td>
<td style="text-align:right;">
0.4368
</td>
</tr>
<tr>
<td style="text-align:left;">
Poland
</td>
<td style="text-align:right;">
0.2783
</td>
<td style="text-align:right;">
9.3094
</td>
<td style="text-align:right;">
0.1873
</td>
</tr>
<tr>
<td style="text-align:left;">
Korea, Dem. People’s Rep. 
</td>
<td style="text-align:right;">
-1.1072
</td>
<td style="text-align:right;">
4.4920
</td>
<td style="text-align:right;">
0.5255
</td>
</tr>
<tr>
<td style="text-align:left;">
Portugal
</td>
<td style="text-align:right;">
2.9462
</td>
<td style="text-align:right;">
3.6178
</td>
<td style="text-align:right;">
0.4682
</td>
</tr>
<tr>
<td style="text-align:left;">
Paraguay
</td>
<td style="text-align:right;">
3.4923
</td>
<td style="text-align:right;">
0.5356
</td>
<td style="text-align:right;">
0.4522
</td>
</tr>
<tr>
<td style="text-align:left;">
French Polynesia
</td>
<td style="text-align:right;">
3.2425
</td>
<td style="text-align:right;">
2.1754
</td>
<td style="text-align:right;">
0.3394
</td>
</tr>
<tr>
<td style="text-align:left;">
Qatar
</td>
<td style="text-align:right;">
4.2834
</td>
<td style="text-align:right;">
53.6178
</td>
<td style="text-align:right;">
0.3925
</td>
</tr>
<tr>
<td style="text-align:left;">
Romania
</td>
<td style="text-align:right;">
0.3418
</td>
<td style="text-align:right;">
5.7855
</td>
<td style="text-align:right;">
0.3468
</td>
</tr>
<tr>
<td style="text-align:left;">
Russian Federation
</td>
<td style="text-align:right;">
-0.0123
</td>
<td style="text-align:right;">
16.7677
</td>
<td style="text-align:right;">
0.3554
</td>
</tr>
<tr>
<td style="text-align:left;">
Rwanda
</td>
<td style="text-align:right;">
1.7467
</td>
<td style="text-align:right;">
0.0610
</td>
<td style="text-align:right;">
0.4857
</td>
</tr>
<tr>
<td style="text-align:left;">
Saudi Arabia
</td>
<td style="text-align:right;">
6.0276
</td>
<td style="text-align:right;">
12.6174
</td>
<td style="text-align:right;">
0.4304
</td>
</tr>
<tr>
<td style="text-align:left;">
Senegal
</td>
<td style="text-align:right;">
1.8644
</td>
<td style="text-align:right;">
0.4258
</td>
<td style="text-align:right;">
0.3271
</td>
</tr>
<tr>
<td style="text-align:left;">
Singapore
</td>
<td style="text-align:right;">
3.7619
</td>
<td style="text-align:right;">
9.6861
</td>
<td style="text-align:right;">
0.4664
</td>
</tr>
<tr>
<td style="text-align:left;">
Solomon Islands
</td>
<td style="text-align:right;">
1.9302
</td>
<td style="text-align:right;">
0.3566
</td>
<td style="text-align:right;">
0.3393
</td>
</tr>
<tr>
<td style="text-align:left;">
Sierra Leone
</td>
<td style="text-align:right;">
-1.2901
</td>
<td style="text-align:right;">
0.1521
</td>
<td style="text-align:right;">
0.4071
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador
</td>
<td style="text-align:right;">
2.9284
</td>
<td style="text-align:right;">
0.6683
</td>
<td style="text-align:right;">
0.4617
</td>
</tr>
<tr>
<td style="text-align:left;">
Somalia
</td>
<td style="text-align:right;">
0.7106
</td>
<td style="text-align:right;">
0.0754
</td>
<td style="text-align:right;">
0.4613
</td>
</tr>
<tr>
<td style="text-align:left;">
Sao Tome and Principe
</td>
<td style="text-align:right;">
2.2521
</td>
<td style="text-align:right;">
0.3712
</td>
<td style="text-align:right;">
0.3869
</td>
</tr>
<tr>
<td style="text-align:left;">
Suriname
</td>
<td style="text-align:right;">
1.2875
</td>
<td style="text-align:right;">
4.1732
</td>
<td style="text-align:right;">
0.2841
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:right;">
-0.7318
</td>
<td style="text-align:right;">
7.1811
</td>
<td style="text-align:right;">
0.2770
</td>
</tr>
<tr>
<td style="text-align:left;">
Eswatini
</td>
<td style="text-align:right;">
4.3015
</td>
<td style="text-align:right;">
0.8260
</td>
<td style="text-align:right;">
0.3758
</td>
</tr>
<tr>
<td style="text-align:left;">
Syrian Arab Republic
</td>
<td style="text-align:right;">
1.5326
</td>
<td style="text-align:right;">
2.2426
</td>
<td style="text-align:right;">
0.4145
</td>
</tr>
<tr>
<td style="text-align:left;">
Chad
</td>
<td style="text-align:right;">
2.4157
</td>
<td style="text-align:right;">
0.0510
</td>
<td style="text-align:right;">
0.3722
</td>
</tr>
<tr>
<td style="text-align:left;">
Togo
</td>
<td style="text-align:right;">
4.1146
</td>
<td style="text-align:right;">
0.2191
</td>
<td style="text-align:right;">
0.4703
</td>
</tr>
<tr>
<td style="text-align:left;">
Thailand
</td>
<td style="text-align:right;">
6.2834
</td>
<td style="text-align:right;">
1.8140
</td>
<td style="text-align:right;">
0.7679
</td>
</tr>
<tr>
<td style="text-align:left;">
Tonga
</td>
<td style="text-align:right;">
3.5631
</td>
<td style="text-align:right;">
0.6759
</td>
<td style="text-align:right;">
0.5748
</td>
</tr>
<tr>
<td style="text-align:left;">
Trinidad and Tobago
</td>
<td style="text-align:right;">
4.2812
</td>
<td style="text-align:right;">
17.1814
</td>
<td style="text-align:right;">
0.5816
</td>
</tr>
<tr>
<td style="text-align:left;">
Tunisia
</td>
<td style="text-align:right;">
3.3723
</td>
<td style="text-align:right;">
1.5716
</td>
<td style="text-align:right;">
0.4326
</td>
</tr>
<tr>
<td style="text-align:left;">
Turkey
</td>
<td style="text-align:right;">
3.6945
</td>
<td style="text-align:right;">
2.4980
</td>
<td style="text-align:right;">
0.4709
</td>
</tr>
<tr>
<td style="text-align:left;">
Tanzania
</td>
<td style="text-align:right;">
1.8145
</td>
<td style="text-align:right;">
0.1244
</td>
<td style="text-align:right;">
0.3628
</td>
</tr>
<tr>
<td style="text-align:left;">
Uganda
</td>
<td style="text-align:right;">
1.4977
</td>
<td style="text-align:right;">
0.0786
</td>
<td style="text-align:right;">
0.4399
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:right;">
0.2672
</td>
<td style="text-align:right;">
1.7782
</td>
<td style="text-align:right;">
0.1969
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:right;">
-0.0564
</td>
<td style="text-align:right;">
19.0121
</td>
<td style="text-align:right;">
0.0924
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Vincent and the Grenadines
</td>
<td style="text-align:right;">
4.9290
</td>
<td style="text-align:right;">
0.9817
</td>
<td style="text-align:right;">
0.7709
</td>
</tr>
<tr>
<td style="text-align:left;">
Venezuela, RB
</td>
<td style="text-align:right;">
-0.4319
</td>
<td style="text-align:right;">
6.0106
</td>
<td style="text-align:right;">
0.1112
</td>
</tr>
<tr>
<td style="text-align:left;">
Vietnam
</td>
<td style="text-align:right;">
4.4265
</td>
<td style="text-align:right;">
0.6517
</td>
<td style="text-align:right;">
0.8192
</td>
</tr>
<tr>
<td style="text-align:left;">
Samoa
</td>
<td style="text-align:right;">
4.0731
</td>
<td style="text-align:right;">
0.6365
</td>
<td style="text-align:right;">
0.4885
</td>
</tr>
<tr>
<td style="text-align:left;">
Yemen, Rep. 
</td>
<td style="text-align:right;">
6.5750
</td>
<td style="text-align:right;">
0.4488
</td>
<td style="text-align:right;">
0.8496
</td>
</tr>
<tr>
<td style="text-align:left;">
South Africa
</td>
<td style="text-align:right;">
0.7035
</td>
<td style="text-align:right;">
8.1874
</td>
<td style="text-align:right;">
0.1465
</td>
</tr>
</tbody>
</table>

We can see which country has the highest and the lowest emission change
in the long run.

``` r
> #country with the minimum emission change 
> emission[which.min(emission$change),]
```

    # A tibble: 1 × 4
    # Groups:   country [1]
      country change  mean    cv
      <fct>    <dbl> <dbl> <dbl>
    1 Aruba    -5.54  105. 0.996

``` r
> #country with the maximum emission change
> emission[which.max(emission$change),]
```

    # A tibble: 1 × 4
    # Groups:   country [1]
      country              change  mean    cv
      <fct>                 <dbl> <dbl> <dbl>
    1 United Arab Emirates   9.77  31.0 0.681

## **Probability distribution of the change rate of CO<sub>2</sub> emissions per capita in the long run:**

- Let’s have a closer look to our variable distribution:

``` r
#plot the empirical probability density function.
epdfPlot(emission$change,xlab=expression('Change of CO'[2] ~ 'emissions per capita in the long run'),main=expression('Empirical PDF of the percentage change of CO'[2]~ 'emissions per capita in the long run'),cex.main=0.95)
abline(v=mean(emission$change),col="darkgray")
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-5-1.png?raw=true)<!-- -->

The dark gray vertical line represents the mean in our data, as you
might expected from the code.

``` r
#plot of the empirical cumulative distribution function.
ecdfPlot(emission$change, xlab =expression('Order Statistics for the change rate of CO'[2]~'emissions per capita'),main=expression('Empirical CDF of the percentage change of CO'[2]~ 'emissions per capita in the long run'),cex.main=0.95)
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-6-1.png?raw=true)<!-- -->

We can see that the probability that the emission change rate in the
long run is less than or equal to zero is almost
![0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;0 "0"),
supporting our hypothesis that the change rate is positive.
Simply, let
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
be the change of CO<sub>2</sub> emissions per capita, then
![P(X \le 0) \approx 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;P%28X%20%5Cle%200%29%20%5Capprox%200 "P(X \le 0) \approx 0"),
but we didn’t check yet for its significance from zero, which comes
later.

Now, let’s “boxplot” it!

``` r
boxplot(emission$change,horizontal=T,xlab=expression('Change of CO'[2] ~ 'emissions per capita in the long run'),varwidth=T)
abline(v=0)
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/unnamed-chunk-7-1.png?raw=true)<!-- -->

It doesn’t seem close to zero but it might not be significant, so let’s
test if the change in the long run across countries is significant from
zero.

``` r
#using t-test.
t.test(emission$change)
```


        One Sample t-test

    data:  emission$change
    t = 14.803, df = 152, p-value < 2.2e-16
    alternative hypothesis: true mean is not equal to 0
    95 percent confidence interval:
     2.149774 2.811979
    sample estimates:
    mean of x 
     2.480876 

The p-value is extremely small almost equal to
![0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;0 "0"),
thus the change in the long run is significant from zero at all levels.
In other words, the carbon dioxide emissions per capita, in metric tons,
across countries over the period
![1960-2016](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1960-2016 "1960-2016")
increased significantly by more than
![2\\%](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;2%5C%25 "2\%"),
on average.

We can play more with our data, but I’ll leave that to you, which is why
I inserted the interactive table.

Have fun!
