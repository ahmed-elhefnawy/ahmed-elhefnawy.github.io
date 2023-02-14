---
title: "k-means"
excerpt: "Application of k-means using `cars` dataset"
collection: portfolio
---


``` r
#read and assign the data. I made sure that the data file is in the same working directory as the working directory in R.
library(data.table)
dt_cars <- fread(file = "cars.csv", sep = ";")

#create a variable for the Drive type.
dt_cars[, "Drive" := "FWD"][AWD==1, "Drive" := "AWD"][RWD==1, "Drive" := "RWD"][, "Drive" := as.factor(Drive)]

#omit the missing values from the dataset. Essential later to get k-means as the function cannot handle the missing values/require data without missing values.
dt_cars<-na.omit(dt_cars)
```

------------------------------------------------------------------------

$\bullet$ **k-Means:**

``` r
#get the k-means; the mean for each cluster based on the three variables inserted, with two clusters in total.
k_means<-kmeans(dt_cars[,c("HP","EngineSize","Length")],2)

k_means
```

    ## K-means clustering with 2 clusters of sizes 265, 122
    ## 
    ## Cluster means:
    ##         HP EngineSize   Length
    ## 1 177.1585   2.702642 182.2151
    ## 2 295.4344   4.050000 190.9262
    ## 
    ## Clustering vector:
    ##   [1] 1 1 2 2 1 2 1 1 1 1 1 1 1 1 2 1 1 1 2 2 2 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1
    ##  [38] 2 2 2 2 2 1 2 1 1 1 1 1 1 2 2 2 1 1 2 2 2 2 2 2 2 1 1 1 1 1 1 2 2 1 1 2 1
    ##  [75] 1 1 1 1 1 2 2 1 2 1 1 2 2 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1
    ## [112] 2 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 2 2 1 2 1 2 1 2 1 1 1 1 1 1 1 1 1 2 2 2 2
    ## [149] 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 1 1 1 1
    ## [186] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 1 1 1 1 2 1 1 2 2 1 1 2 2 2 2 2 2 1 1 1
    ## [223] 1 1 1 1 1 1 2 1 1 1 2 2 1 2 1 1 2 2 2 2 2 2 2 2 2 1 2 1 1 1 2 1 1 1 1 1 1
    ## [260] 1 1 1 1 1 1 2 1 1 2 2 1 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2
    ## [297] 2 2 2 1 2 2 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1
    ## [334] 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1
    ## [371] 1 1 2 2 1 2 1 1 1 2 2 1 1 2 1 1 2
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 436886.2 361635.7
    ##  (between_SS / total_SS =  59.5 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

`kmeans` divides the dataset, given the horse power, engine size and
length, into two clusters: the first one with 122 observations, and the
second part with 265 observations.

`Cluster means` shows the mean for each variable per cluster.

`Clustering vector` shows the number of cluster, for each observation.
If the observation belongs to the first cluster or the second one.

`Within cluster sum of squares by cluster` shows the sum of squares per
cluster.

`Available components:` \[from
[documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html)\]

`cluster` : a vector of integers (from 1:k) indicating the cluster to
which each point is allocated, which was shown in the
`Clustering vector`; `centers`: a matrix of cluster centres; `totss`:
The total sum of squares; `withinss`: vector of within-cluster sum of
squares, one component per cluster, which was shown before;
`tot.withinss`: total within-cluster sum of squares, i.e. sum(withinss);
`betweenss`: The between-cluster sum of squares,
i.e. totss-tot.withinss; `size`: The number of points in each cluster;
`iter`: The number of (outer) iterations; `ifault`: integer: indicator
of a possible algorithm problem – for experts.

We can retrieve the total within sum of squares (total WSS) by
retrieving the `tot.withinss` component:

``` r
k_means$tot.withinss
```

    ## [1] 798521.9

------------------------------------------------------------------------

$\bullet$ **Rerun kmeans for k=1 to 15, with elbow plot:**

``` r
#I use "lapply" to run the function 15 times for k=1 to 15.
k15_all<-lapply(1:15, function(k){
  kmeans(dt_cars[,c("HP","EngineSize","Length")],k)
})


#Another way is to create a loop to run the code 15 times for k=1 to 15.

#k15_all<-for(k in 1:15){     #specify the range for k.
#k15<-print(kmeans(dt_cars[,c("HP","EngineSize","Length")],k))
#}

#alternatively, we can use "map_dbl" from "purrr" package.
library(purrr)
```

    ## 
    ## Attaching package: 'purrr'

    ## The following object is masked from 'package:data.table':
    ## 
    ##     transpose

``` r
k15_alt<-map_dbl(1:15,function(k){ #create a function for k, to cover the range 1:15.
  meansk<-kmeans(dt_cars[,c("HP","EngineSize","Length")],k) #the same function as before, using kmeans.
  meansk$tot.withinss #extracting the total within sum of squares.
})

#create a new dataframe to store the output of the previous function, so that it can be plotted later.
new_df<-data.frame(k=1:15, tot_withinss=k15_alt) #the "tot_withinss" will retrieve the total within sum of squares, which was extracted in the previous function using: "meansk$tot.withinss" in the previous function.

#now we plot the new dataframe. "type" here will display both: line and points. "xlim" is for the limits of the x-axis.
plot(x=new_df$k,y=new_df$tot_withinss,type="b",xlim=c(1,15),xlab="k",ylab="Total within sum of squares")
axis(side=1,at=1:15) #I use "axis" function to display the whole range from 1 to 15, and with a break of one.


#another way is to use "fviz_nbclust" from "factoextra" package, which will plot directly the clusters against the total within sum of squares, for the aim to specify the optimal number of clusters by showing the "elbow" form in the plot.  
library(factoextra)
```

    ## Loading required package: ggplot2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-4-1.png?raw=true)<!-- -->

``` r
#first we specify the data, then I specify it's kmeans for clustering, then the maximum number of clusters (k.max), then the method that should be used, and here: "wss" is for total within sum of squares.
fviz_nbclust(dt_cars[,c("HP","EngineSize","Length")], kmeans, k.max=15,method = "wss")
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-4-2.png?raw=true)<!-- -->

------------------------------------------------------------------------

$\bullet$ **Change the random starting locations:**

``` r
#I create a loop to plot three plots directly with 3 different starting locations; at 1, which is already the default, 10, and 100.
for(i in c(1,10,100)){
  print(fviz_nbclust(dt_cars[,c("HP","EngineSize","Length")], kmeans, k.max=15,method = "wss",nstart=i))
}
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-5-1.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-5-2.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-5-3.png?raw=true)<!-- -->

The three plots, almost, look exactly the same.

------------------------------------------------------------------------

------------------------------------------------------------------------

$\bullet$ **Determine the optimal k for k-means clustering:**

``` r
library(NbClust)

#run NbClust to determine the best number of clusters.
NbClust(dt_cars[,c("HP","EngineSize","Length")],method="kmeans")
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-6-1.png?raw=true)<!-- -->

    ## *** : The Hubert index is a graphical method of determining the number of clusters.
    ##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
    ##                 significant increase of the value of the measure i.e the significant peak in Hubert
    ##                 index second differences plot. 
    ## 

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-6-2.png?raw=true)<!-- -->

    ## *** : The D index is a graphical method of determining the number of clusters. 
    ##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
    ##                 second differences plot) that corresponds to a significant increase of the value of
    ##                 the measure. 
    ##  
    ## ******************************************************************* 
    ## * Among all indices:                                                
    ## * 4 proposed 2 as the best number of clusters 
    ## * 5 proposed 3 as the best number of clusters 
    ## * 6 proposed 4 as the best number of clusters 
    ## * 3 proposed 7 as the best number of clusters 
    ## * 1 proposed 11 as the best number of clusters 
    ## * 2 proposed 13 as the best number of clusters 
    ## * 2 proposed 15 as the best number of clusters 
    ## 
    ##                    ***** Conclusion *****                            
    ##  
    ## * According to the majority rule, the best number of clusters is  4 
    ##  
    ##  
    ## *******************************************************************

    ## $All.index
    ##        KL       CH Hartigan     CCC    Scott      Marriot       TrCovW
    ## 2  1.7658 566.5874 351.9790 48.0143 2532.783 1.813855e+13 168310707762
    ## 3  1.8676 716.4136 233.5676 44.4783 2869.946 1.707723e+13  43251289545
    ## 4  3.5862 843.7671 106.1104 46.9908 3103.666 1.659636e+13  13638612241
    ## 5  0.6160 832.4982 133.4959 46.4548 3240.410 1.821283e+13   7906602171
    ## 6  2.0412 923.0188  85.8391 48.2108 3393.060 1.767806e+13   3862808487
    ## 7  8.4094 954.2740  30.2263 48.6865 3521.287 1.727555e+13   2754191211
    ## 8  0.2649 884.9942  51.0977 47.0867 3601.140 1.835706e+13   2254093646
    ## 9  3.3104 882.8241  21.5763 46.9110 3698.818 1.805069e+13   1685939877
    ## 10 0.1769 829.7217  60.9215 45.5944 3740.843 1.999162e+13   1441458648
    ## 11 3.8146 871.1960  13.2632 46.4321 3864.851 1.755780e+13   1112542532
    ## 12 0.5719 818.9554  37.1824 45.1331 3900.596 1.905172e+13   1111370767
    ## 13 0.4506 826.0340 -14.5896 45.2055 3985.140 1.797142e+13    964198129
    ## 14 1.4721 729.6699  41.5903 42.7111 3954.510 2.255927e+13   1132016087
    ## 15 1.6556 754.0428  31.3471 43.2596 4071.028 1.916434e+13    931856447
    ##       TraceW  Friedman    Rubin Cindex     DB Silhouette    Duda Pseudot2
    ## 2  798521.87  415.0928  41.3435 0.1953 0.8014     0.5351  0.7126  84.2939
    ## 3  417150.15  455.8464  79.1411 0.1503 0.7314     0.5233  1.2195 -23.9428
    ## 4  259381.56  506.3640 127.2786 0.2790 0.6174     0.5369  1.1897 -29.6563
    ## 5  203109.86  553.7486 162.5412 0.2583 0.7197     0.4523  0.7499  54.0219
    ## 6  150511.32  635.6525 219.3438 0.2240 0.7270     0.4548  1.3950 -48.7051
    ## 7  122836.35  701.7925 268.7619 0.1948 0.8048     0.4248  0.6942  29.9568
    ## 8  113785.51  732.0345 290.1400 0.1893 0.8202     0.4030  0.9229  10.1064
    ## 9  100267.23  791.9223 329.2574 0.1721 0.8608     0.3966  1.4361 -29.1518
    ## 10  94853.01  837.6294 348.0514 0.1683 0.8976     0.3976  0.8769  11.7973
    ## 11  81657.53  959.5681 404.2949 0.1503 0.8895     0.3929  4.2342 -54.9958
    ## 12  78875.25  989.0155 418.5562 0.1486 0.9305     0.3775  2.7872 -37.8315
    ## 13  71760.03 1109.4095 460.0573 0.1377 0.8882     0.3952  0.5241  40.8651
    ## 14  74672.99 1158.0305 442.1107 0.1462 0.8567     0.3997 28.9063 -70.4746
    ## 15  67182.04 1236.0978 491.4070 0.1338 0.8931     0.3971  0.5485  37.0346
    ##      Beale Ratkowsky       Ball Ptbiserial   Frey McClain   Dunn Hubert SDindex
    ## 2   0.6840    0.4024 399260.934     0.5693 0.8200  0.3187 0.0200      0  0.1557
    ## 3  -0.3040    0.4136 139050.050     0.5747 0.0630  0.5629 0.0136      0  0.1079
    ## 4  -0.2693    0.3655  64845.389     0.5922 2.0254  0.5493 0.0413      0  0.0964
    ## 5   0.5628    0.3347  40621.971     0.5249 0.5948  0.7580 0.0218      0  0.1077
    ## 6  -0.4769    0.3094  25085.220     0.5110 1.3725  0.8074 0.0136      0  0.0977
    ## 7   0.7375    0.2945  17548.050     0.4553 1.8882  1.0263 0.0392      0  0.1291
    ## 8   0.1404    0.2824  14223.189     0.4376 1.2866  1.1139 0.0277      0  0.1524
    ## 9  -0.5085    0.2707  11140.804     0.3976 1.7607  1.3341 0.0291      0  0.1648
    ## 10  0.2353    0.2607   9485.301     0.3838 0.6522  1.4296 0.0190      0  0.1817
    ## 11 -1.2412    0.2569   7423.412     0.3609 4.3162  1.5534 0.0190      0  0.1778
    ## 12 -1.0604    0.2486   6572.938     0.3419 0.4712  1.7364 0.0291      0  0.2617
    ## 13  1.4816    0.2420   5520.002     0.3307 0.1668  1.7920 0.0190      0  0.2360
    ## 14 -1.4609    0.2319   5333.785     0.3324 0.7051  1.8293 0.0213      0  0.2623
    ## 15  1.3685    0.2294   4478.803     0.3103 0.5544  2.0198 0.0213      0  0.3319
    ##     Dindex   SDbw
    ## 2  37.2800 1.4464
    ## 3  25.7386 0.6170
    ## 4  22.8905 0.3853
    ## 5  20.3485 0.3777
    ## 6  17.4595 0.3467
    ## 7  15.4729 0.3219
    ## 8  14.7556 0.3089
    ## 9  13.5872 0.2726
    ## 10 13.0335 0.1871
    ## 11 12.0775 0.2029
    ## 12 11.6815 0.2044
    ## 13 11.1682 0.2290
    ## 14 11.2894 0.1668
    ## 15 10.5051 0.1224
    ## 
    ## $All.CriticalValues
    ##    CritValue_Duda CritValue_PseudoT2 Fvalue_Beale
    ## 2          0.6507           112.2034       0.5620
    ## 3          0.5897            92.5275       1.0000
    ## 4          0.5889           129.8194       1.0000
    ## 5          0.5823           116.2004       0.6399
    ## 6          0.5588           135.8096       1.0000
    ## 7          0.5020            67.4568       0.5310
    ## 8          0.5398           103.1634       0.9357
    ## 9          0.5043            94.3556       1.0000
    ## 10         0.5088            81.0996       0.8717
    ## 11         0.3224           151.3437       1.0000
    ## 12         0.4158            82.9018       1.0000
    ## 13         0.3414            86.8187       0.2272
    ## 14         0.0819           817.8037       1.0000
    ## 15         0.4513            54.7130       0.2555
    ## 
    ## $Best.nc
    ##                     KL      CH Hartigan     CCC    Scott      Marriot
    ## Number_clusters 7.0000   7.000   4.0000  7.0000   3.0000 1.300000e+01
    ## Value_Index     8.4094 954.274 127.4573 48.6865 337.1635 5.668137e+12
    ##                       TrCovW   TraceW Friedman    Rubin  Cindex     DB
    ## Number_clusters            3      3.0  11.0000  13.0000 15.0000 4.0000
    ## Value_Index     125059418217 223603.1 121.9387 -59.4477  0.1338 0.6174
    ##                 Silhouette   Duda PseudoT2 Beale Ratkowsky     Ball PtBiserial
    ## Number_clusters     4.0000 2.0000   2.0000 2.000    3.0000      3.0     4.0000
    ## Value_Index         0.5369 0.7126  84.2939 0.684    0.4136 260210.9     0.5922
    ##                 Frey McClain   Dunn Hubert SDindex Dindex    SDbw
    ## Number_clusters    1  2.0000 4.0000      0  4.0000      0 15.0000
    ## Value_Index       NA  0.3187 0.0413      0  0.0964      0  0.1224
    ## 
    ## $Best.partition
    ##   [1] 2 2 3 3 2 3 2 1 2 2 2 2 2 1 2 2 2 2 3 3 4 3 3 1 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [38] 3 3 3 3 3 2 3 2 2 1 2 2 2 2 3 2 2 2 2 3 3 3 3 3 3 2 1 1 1 1 1 3 3 2 2 2 1
    ##  [75] 2 2 2 2 2 3 3 1 3 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 3 1 2 2 2 2 1 1 1 1 2 2
    ## [112] 2 2 2 2 1 1 1 1 1 1 2 2 3 1 1 2 3 3 2 3 1 2 1 2 1 1 1 1 1 1 1 1 1 2 2 2 2
    ## [149] 3 1 1 1 1 1 1 1 1 1 1 2 2 3 3 3 3 3 2 3 3 3 2 2 3 4 3 3 4 3 3 4 4 2 2 2 1
    ## [186] 2 1 1 1 1 1 2 2 1 1 1 2 1 3 2 2 3 2 2 2 2 3 2 2 3 3 2 2 3 3 3 2 2 2 2 1 1
    ## [223] 1 1 2 1 1 1 3 2 2 2 3 4 2 3 2 2 3 3 3 3 3 3 3 4 4 2 3 2 2 2 3 2 2 1 1 2 1
    ## [260] 1 2 2 2 2 2 3 2 1 3 3 1 2 3 3 2 3 2 2 2 1 1 1 2 1 1 2 2 1 2 2 2 2 1 1 1 3
    ## [297] 3 4 3 2 3 3 2 2 2 2 2 2 2 1 1 2 1 1 1 1 1 1 1 1 1 2 3 1 1 1 2 2 1 1 1 1 1
    ## [334] 1 1 1 2 2 2 2 1 2 1 2 2 2 1 1 1 1 1 1 1 2 3 1 1 1 1 2 2 2 1 1 1 2 1 1 1 1
    ## [371] 1 2 3 3 2 2 2 1 2 3 2 2 2 3 1 2 3

The output shows many information including the indices used to
determine the optimal number of clusters. In particular, the results
show the [Hubert index](https://doi.org/10.1007/BF01908075) and the [D
index](https://isbnsearch.org/isbn/9782100496167). Given that and all
other indices, the function concludes with the best number of clusters
as 4, according to the majority rule, and that is consistent with the
elbow form in the plot, which is at k=4.

The function provides 30 indices to determine the number of clusters,
and it provides this “by varying all combinations of number of clusters,
distance measures, and clustering methods”. The
[documentation](https://cran.r-project.org/web/packages/NbClust/NbClust.pdf)
provides further information about the indices and the reference(s) for
each of them.

------------------------------------------------------------------------

$\bullet$ **Standardization of variables:**

``` r
#standardize the variables.
dt_cars[,"HP_st":=((HP-mean(HP))/sd(HP))][,"EngineSize_st":=((EngineSize-mean(EngineSize))/sd(EngineSize))][,"Length_st":=((Length-mean(Length))/sd(Length))]

#rerun NbClust to determine the optimal number of clusters, with the standardized variables.
NbClust(dt_cars[,c("HP_st","EngineSize_st","Length_st")],method="kmeans")
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-7-1.png?raw=true)<!-- -->

    ## *** : The Hubert index is a graphical method of determining the number of clusters.
    ##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
    ##                 significant increase of the value of the measure i.e the significant peak in Hubert
    ##                 index second differences plot. 
    ## 

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-7-2.png?raw=true)<!-- -->

    ## *** : The D index is a graphical method of determining the number of clusters. 
    ##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
    ##                 second differences plot) that corresponds to a significant increase of the value of
    ##                 the measure. 
    ##  
    ## ******************************************************************* 
    ## * Among all indices:                                                
    ## * 9 proposed 2 as the best number of clusters 
    ## * 5 proposed 3 as the best number of clusters 
    ## * 2 proposed 4 as the best number of clusters 
    ## * 1 proposed 6 as the best number of clusters 
    ## * 1 proposed 11 as the best number of clusters 
    ## * 4 proposed 12 as the best number of clusters 
    ## * 1 proposed 15 as the best number of clusters 
    ## 
    ##                    ***** Conclusion *****                            
    ##  
    ## * According to the majority rule, the best number of clusters is  2 
    ##  
    ##  
    ## *******************************************************************

    ## $All.index
    ##         KL       CH Hartigan     CCC     Scott  Marriot    TrCovW   TraceW
    ## 2   5.2545 374.1876 143.8735 -3.5475  431.7012 17277644 31799.230 587.2462
    ## 3   5.4984 328.0926  85.3077 -4.9932  754.9692 16861379 28153.934 427.4931
    ## 4   0.5301 294.9859  54.1208 -5.4829  914.3605 19856391 10758.448 349.7861
    ## 5   0.4115 265.3378  67.3353 -6.2822 1035.8950 22663838  8508.747 306.4784
    ## 6   4.8364 262.4654  44.9042 -5.4154 1204.8659 21089943  7443.060 260.5510
    ## 7   1.9193 251.3221  37.2676 -5.2731 1320.6453 21283370  5960.566 233.0804
    ## 8   0.1395 241.2331  18.0177 -6.3561 1426.8380 21127787  4828.974 212.2632
    ## 9   0.8359 222.7765  43.4210 -7.3288 1500.7367 22091711  4527.453 202.6301
    ## 10  3.1836 224.9983  29.7465 -6.2973 1612.5672 20429008  3667.217 181.7522
    ## 11  0.1706 220.8635  53.9150 -6.0332 1696.7407 19887147  2785.867 168.4602
    ## 12 14.5279 233.8535  18.4162 -4.0245 1843.6906 16189789  1954.853 147.3338
    ## 13  1.8161 225.8240  20.1147 -4.2909 1883.4728 17144369  2084.705 140.4370
    ## 14  0.1746 220.6199  10.9908 -4.3286 1943.6834 17018532  2036.245 133.2694
    ## 15  0.5216 211.1153  29.2881 -4.9089 2014.9861 16249201  1994.615 129.4549
    ##    Friedman  Rubin Cindex     DB Silhouette   Duda Pseudot2   Beale Ratkowsky
    ## 2    4.7470 1.9719 0.2821 1.0130     0.4096 1.0308  -6.1560 -0.0506    0.4941
    ## 3    7.4541 2.7088 0.2597 1.1218     0.3549 1.5476 -59.0918 -0.5976    0.4563
    ## 4    8.3923 3.3106 0.2769 1.1492     0.3149 1.2210 -29.1398 -0.3054    0.4175
    ## 5    9.4690 3.7784 0.3000 1.2106     0.3035 1.1591 -23.7477 -0.2315    0.3833
    ## 6   11.2491 4.4444 0.2623 1.0761     0.3187 1.0405  -5.9201 -0.0657    0.3593
    ## 7   12.6561 4.9682 0.2554 1.1325     0.2940 1.9785 -50.4462 -0.8304    0.3377
    ## 8   14.1593 5.4555 0.2536 1.1582     0.2837 2.1886 -65.7128 -0.9083    0.3194
    ## 9   15.4931 5.7148 0.2425 1.2140     0.2564 0.9118   7.9299  0.1621    0.3027
    ## 10  17.1874 6.3713 0.2308 1.1232     0.2794 1.0679  -5.2150 -0.1067    0.2903
    ## 11  18.5901 6.8740 0.2209 1.0874     0.3107 1.8865 -32.4234 -0.7805    0.2787
    ## 12  21.3285 7.8597 0.2294 1.1134     0.3267 2.7881 -39.7625 -1.0631    0.2697
    ## 13  21.8995 8.2457 0.2593 1.0427     0.3154 0.5880  39.2338  1.0734    0.2600
    ## 14  23.2178 8.6892 0.2525 1.0217     0.3178 3.7198 -41.6767 -1.1618    0.2514
    ## 15  25.5824 8.9452 0.2689 1.0474     0.3064 1.9457 -30.1351 -0.7898    0.2433
    ##        Ball Ptbiserial    Frey McClain   Dunn Hubert SDindex Dindex   SDbw
    ## 2  293.6231     0.5180  0.4468  0.5587 0.0261 0.0011  2.7761 1.1062 1.8711
    ## 3  142.4977     0.5563  0.9106  0.8482 0.0251 0.0016  2.7997 0.9317 0.7849
    ## 4   87.4465     0.5222  0.6072  1.1901 0.0360 0.0016  2.4728 0.8625 1.4089
    ## 5   61.2957     0.5081  0.2352  1.3879 0.0343 0.0017  2.7000 0.8000 0.7135
    ## 6   43.4252     0.5123  1.2163  1.4933 0.0601 0.0019  2.6667 0.7466 0.4264
    ## 7   33.2972     0.4555  0.7738  2.0318 0.0418 0.0020  3.1202 0.6935 0.4749
    ## 8   26.5329     0.4309  1.2466  2.3469 0.0308 0.0021  3.3765 0.6671 0.4687
    ## 9   22.5145     0.4099  0.3166  2.6383 0.0416 0.0021  3.9564 0.6389 0.4322
    ## 10  18.1752     0.3963  0.5407  2.9044 0.0217 0.0022  3.8276 0.6061 0.2970
    ## 11  15.3146     0.3773 -0.1033  3.2514 0.0463 0.0022  3.7027 0.5780 0.2283
    ## 12  12.2778     0.3892 -0.4777  3.0615 0.0479 0.0023  3.6958 0.5491 0.2320
    ## 13  10.8028     0.3942  0.3715  2.9811 0.0272 0.0023  3.3982 0.5436 0.1826
    ## 14   9.5192     0.3873  1.9840  3.0957 0.0513 0.0023  3.6301 0.5249 0.2069
    ## 15   8.6303     0.3576  0.1429  3.6647 0.0558 0.0023  4.1897 0.5138 0.1608
    ## 
    ## $All.CriticalValues
    ##    CritValue_Duda CritValue_PseudoT2 Fvalue_Beale
    ## 2          0.6311           120.3975       1.0000
    ## 3          0.5905           115.8094       1.0000
    ## 4          0.5797           116.7533       1.0000
    ## 5          0.5710           129.9800       1.0000
    ## 6          0.5720           113.7295       1.0000
    ## 7          0.5283            91.0649       1.0000
    ## 8          0.4947           123.5891       1.0000
    ## 9          0.5151            77.1919       0.9217
    ## 10         0.5191            75.9709       1.0000
    ## 11         0.4434            86.6240       1.0000
    ## 12         0.4304            82.0604       1.0000
    ## 13         0.1148           431.8191       0.3769
    ## 14         0.2298           191.0699       1.0000
    ## 15         0.3224           130.3238       1.0000
    ## 
    ## $Best.nc
    ##                      KL       CH Hartigan     CCC   Scott Marriot   TrCovW
    ## Number_clusters 12.0000   2.0000   3.0000  2.0000   3.000      12     4.00
    ## Value_Index     14.5279 374.1876  58.5658 -3.5475 323.268 4651937 17395.49
    ##                  TraceW Friedman   Rubin  Cindex    DB Silhouette   Duda
    ## Number_clusters  3.0000  12.0000 12.0000 11.0000 2.000     2.0000 2.0000
    ## Value_Index     82.0461   2.7383 -0.5997  0.2209 1.013     0.4096 1.0308
    ##                 PseudoT2   Beale Ratkowsky     Ball PtBiserial Frey McClain
    ## Number_clusters    2.000  2.0000    2.0000   3.0000     3.0000    1  2.0000
    ## Value_Index       -6.156 -0.0506    0.4941 151.1254     0.5563   NA  0.5587
    ##                   Dunn Hubert SDindex Dindex    SDbw
    ## Number_clusters 6.0000      0  4.0000      0 15.0000
    ## Value_Index     0.0601      0  2.4728      0  0.1608
    ## 
    ## $Best.partition
    ##   [1] 2 2 2 2 1 2 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2
    ##  [38] 2 2 2 2 2 1 2 1 1 1 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 1 1 1 1 1 2 2 2 2 2 1
    ##  [75] 2 2 2 2 2 2 2 1 2 1 1 2 2 2 2 1 2 1 1 1 1 1 1 2 2 2 1 2 2 2 2 1 1 1 1 2 2
    ## [112] 2 1 2 2 1 1 1 1 1 1 2 2 2 1 1 2 2 2 2 2 1 2 1 2 1 1 1 1 1 1 1 1 1 2 2 2 1
    ## [149] 2 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 1 1 2 1
    ## [186] 1 1 1 1 1 1 2 1 1 1 1 2 1 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1
    ## [223] 1 1 1 1 1 1 2 1 1 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 1 1 2 1
    ## [260] 1 2 1 1 2 2 1 2 1 2 2 1 2 2 2 2 2 2 2 2 1 1 1 1 1 1 2 1 1 2 2 1 2 1 1 1 2
    ## [297] 2 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [334] 1 1 1 1 2 2 2 1 1 1 2 2 1 1 1 1 1 1 1 1 2 2 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1
    ## [371] 1 1 2 2 2 1 1 1 1 2 1 1 1 2 1 1 2

The optimal number of clusters is 2 instead of 4. The difference after
standardization is because of the change of the variables’ range. Since
the aim is to **minimize** the squared Euclidean distance between the
data points and their nearest prototypes, the range of each variable
will have an effect on determining this distance (e.g. big difference in
magnitude). As the range of a variable gets bigger, the effect of that
variable gets higher because the distance between the data points and
their nearest prototypes gets bigger.

Another important point is the different units of each variable as they
don’t measure the same thing; they are not expressed in the same way
(e.g. percentage), and this is the main reason for the different ranges
among the variables. However, the standardization controls for this
limitation, and leads to more comparability between the variables, and
hence lower number of clusters.

``` r
#plot the number of clusters against the total within sum of squares.
fviz_nbclust(dt_cars[,c("HP_st","EngineSize_st","Length_st")], kmeans, k.max=15,method = "wss")
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-8-1.png?raw=true)<!-- -->

The elbow shape/form can be seen at k=2 as well.

------------------------------------------------------------------------

------------------------------------------------------------------------

$\bullet$ **EM Algorithm to implement k-means clustering:**

``` r
#first I subset the two variables into a matrix, so the calculation can be easier.
newdata<-data.matrix(dt_cars[,c("HP_st","EngineSize_st")])

#then I define some parameters; 
A <- sample(1:2, length(newdata)/2, replace = T) #I create a random sample of the two clusters, with replacement, since the assignment to 1 or two is not unique.

C<-newdata[1:2,] #I subset the first two rows, to be used later as the cluster centers. Any two points can be used as a cluster center.

colors <- seq(2, 3) #create a color parameter with two numbers, for the two clusters, to be used later to color the plots by cluster.

#create a loop to plot HP against Engine size for the two clusters. The initialization stage, as per slides p. 12.
plot(newdata,xlab="HP (standardized)",ylab="Engine Size (standardized)",main="Initialize")
for(i in 1:2) { #1:2 is for the two clusters
    points(C[i, , drop = FALSE], col = colors[i])
    points(newdata[A == i, , drop = FALSE], col = colors[i])    
  }
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-1.png?raw=true)<!-- -->

``` r
#create a loop within a function, similar to the previous one to plot the iterations of the algorithm.
k_plot <- function(newdata, C, txt) {
  plot(newdata, main = txt, xlab = "", ylab = "")
  for(i in 1:2) {
    points(C[i, , drop = FALSE], pch = 23, lwd = 3, col = colors[i]) #the parameters in "points" function are explained in "points" documentation.
    points(newdata[A == i, , drop = FALSE], col = colors[i])    
  }
}

#I repeat the iterations until no data point changes its cluster membership. The loop stops with "break" command.
repeat {
  # calculate Euclidean distance between objects and cluster centers.
  D <- matrix(data = NA, nrow = 2, ncol = length(newdata)/2) #create an empty matrix/with NA values, with two rows and number of columns as our data, to be filled later with the distance calculations.
  for(i in 1:2) { #nested loop; to calculate the distance for each data point, for each cluster. 
    for(j in 1:length(newdata)/2) {
      D[i, j] <- sqrt((newdata[j, 1] - C[i, 1])^2 + (newdata[j, 2] - C[i, 2])^2) 
    }
  }
  O <- A #assign the sampling of the clustering into a new variable, to be used later to check if the data points changed or not.
   
  #E-step: parameters are fixed.
  A <- max.col(t(-D)) # assign objects to cluster centers. "t" to get the transpose of the matrix, after changing the sign, then calculate the maximum.
  if(all(O == A)) break #here is the condition where the algorithm should stop if there is no change.
  
  k_plot(newdata, C, "E-step") #I plot the "E-step" using the function created before.
   
  #M-step: assignments are fixed.
  #determine new cluster centers based on mean of assigned objects
  for(i in 1:2) {
    C[i, ] <- apply(newdata[A == i, , drop = FALSE], 2, mean) #within the loop, "apply" will return the values for the dataset whenever the assigned objects before is equal to the cluster number. the other parameters in "apply" are explained in the documentation. 
  }
  k_plot(newdata, C, "M-step") #plot the M-step.
}
```

![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-2.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-3.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-4.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-5.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-6.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-7.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-8.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-9.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-10.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-11.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-12.png?raw=true)<!-- -->![](https://github.com/ahmed-elhefnawy/ahmed-elhefnawy.github.io/blob/master/images/k-means/unnamed-chunk-9-13.png?raw=true)<!-- -->

After 6 successive E- and M-steps, the algorithm converges to an
optimum, in which all data points stop changing cluster membership
(e.g. clusters are stable).
