Data analysis
================
JasonL
31/03/2022

SECTION 1: Load packages we need.

``` r
library(reshape2)
library(dplyr)
library(plyr)
library(ggplot2)
library(lme4)
library(RLRsim)
```

    ## Warning: package 'RLRsim' was built under R version 4.0.5

``` r
library(car)
```

SECTION 2: Upload the data, name it, and look at it.

``` r
setwd("C:/Users/jason/Dropbox/My PC (DESKTOP-IOO274E)/Desktop/Data/DuckweedExpEvol/Git_repo/Lemna_exp_evol")

data<-read.table('DW_Exp_Evol_final.txt', na.strings="", header=FALSE, sep='\t')

#There are alot of columns to label here. 
names(data)<-c('Pop','Media','Trt','Rep','G1ID','G1Pos','G1Edge','G1_notes0','G1_fr0','G1_pix0','G1_per0','G1_grn0','G1_grnper0','G1_notes1','G1_fr1','G1_pix1','G1_per1','G1_grn1','G1_grnper1','G1_abscnt','G2ID','G2Pos','G2Edge','G2_notes0','G2_fr0','G2_pix0','G2_per0','G2_grn0','G2_grnper0','G2_notes1','G2_fr1','G2_pix1','G2_per1','G2_grn1','G2_grnper1','G2_abscnt','G3ID','G3Pos','G3Edge','G3_notes0','G3_fr0','G3_pix0','G3_per0','G3_grn0','G3_grnper0','G3_notes1','G3_fr1','G3_pix1','G3_per1','G3_grn1','G3_grnper1','G3_abscnt','G4ID','G4Pos','G4Edge','G4_notes0','G4_fr0','G4_pix0','G4_per0','G4_grn0','G4_grnper0','G4_notes1','G4_fr1','G4_pix1','G4_per1','G4_grn1','G4_grnper1','G4_abscnt','G5ID','G5Pos','G5Edge','G5_notes0','G5_fr0','G5_pix0','G5_per0','G5_grn0','G5_grnper0','G5_notes1','G5_fr1','G5_pix1','G5_per1','G5_grn1','G5_grnper1','G5_abscnt','G6ID','G6Pos','G6Edge','G6_notes0','G6_fr0','G6_pix0','G6_per0','G6_grn0','G6_grnper0','G6_notes1','G6_fr1','G6_pix1','G6_per1','G6_grn1','G6_grnper1','G6_abscnt','G7ID','G7Pos','G7Edge','G7_notes0','G7_fr0','G7_pix0','G7_per0','G7_grn0','G7_grnper0','G7_notes1','G7_fr1','G7_pix1','G7_per1','G7_grn1','G7_grnper1','G7_abscnt','G8ID','G8Pos','G8Edge','G8_notes0','G8_fr0','G8_pix0','G8_per0','G8_grn0','G8_grnper0','G8_notes1','G8_fr1','G8_pix1','G8_per1','G8_grn1','G8_grnper1','G8_abscnt','G9ID','G9Pos','G9Edge','G9_notes0','G9_fr0','G9_pix0','G9_per0','G9_grn0','G9_grnper0','G9_notes1','G9_fr1','G9_pix1','G9_per1','G9_grn1','G9_grnper1','G9_abscnt', 'G10ID','G10Pos','G10Edge','G10_notes0','G10_fr0','G10_pix0','G10_per0','G10_grn0','G10_grnper0','G10_notes1','G10_fr1','G10_pix1','G10_per1','G10_grn1','G10_grnper1','G10_abscnt')

head(data)
```

    ##         Pop    Media Trt Rep G1ID G1Pos G1Edge G1_notes0 G1_fr0 G1_pix0 G1_per0
    ## 1 Churchill    SaltK PLT   7    5     5   Edge        NA      2    1399 157.580
    ## 2 Churchill RegularK BAC   2    9     9   Edge        NA     NA      NA      NA
    ## 3 Churchill RegularK PLT   8   10    10 Middle        NA      1    3217 219.622
    ## 4 Churchill    SaltK PLT   2   11    11 Middle        NA      2    2231 186.995
    ## 5 Churchill    SaltK BAC  17   12    12   Edge        NA     NA      NA      NA
    ## 6 Churchill RegularK PLT   2   13    13   Edge        NA      2    1755 179.823
    ##   G1_grn0 G1_grnper0 G1_notes1 G1_fr1 G1_pix1 G1_per1 G1_grn1 G1_grnper1
    ## 1    1124    138.853        NA      2    1810 186.056    1383    166.409
    ## 2      NA         NA        NA     NA      NA      NA      NA         NA
    ## 3    2800    210.208        NA      4    5190 503.814    2812    461.817
    ## 4    1889    179.924        NA      3    2723 284.292    1968    261.806
    ## 5      NA         NA        NA     NA      NA      NA      NA         NA
    ## 6    1498    171.581        NA      4    6056 396.718    5031    503.086
    ##   G1_abscnt G2ID G2Pos G2Edge G2_notes0 G2_fr0 G2_pix0 G2_per0 G2_grn0
    ## 1       198  220     4   Edge        NA      1    2552 228.835    1928
    ## 2       218  219     3   Edge        NA     NA      NA      NA      NA
    ## 3       151  105     9   Edge        NA      2    1281 163.338     969
    ## 4       318   88    16   Edge        NA      2    1486 171.095    1196
    ## 5      1800   26     2   Edge        NA     NA      NA      NA      NA
    ## 6       231   18    18 Middle        NA      1    1420 143.681    1189
    ##   G2_grnper0 G2_notes1 G2_fr1 G2_pix1 G2_per1 G2_grn1 G2_grnper1 G2_abscnt G3ID
    ## 1    173.338      <NA>      4    5519 346.434    4133    365.687      72.6   36
    ## 2         NA      <NA>     NA      NA      NA      NA         NA     543.0  200
    ## 3    128.468      <NA>      4    9011 855.032    7493    500.642     980.0   29
    ## 4    167.238      <NA>      3    3128 279.421    2201    313.161     218.0    7
    ## 5         NA      <NA>     NA      NA      NA      NA         NA     323.0  158
    ## 6    133.439      <NA>      6   12480 839.280    8913    855.116    1135.0  112
    ##   G3Pos G3Edge G3_notes0 G3_fr0 G3_pix0 G3_per0 G3_grn0 G3_grnper0 G3_notes1
    ## 1    12   Edge        NA      2    1428 152.368    1293    145.296        NA
    ## 2     8   Edge        NA     NA      NA      NA      NA         NA        NA
    ## 3     5   Edge        NA      2    4493 279.764    4049    273.421        NA
    ## 4     7 Middle        NA      2    5087 307.220    4611    293.907        NA
    ## 5    14 Middle        NA     NA      NA      NA      NA         NA        NA
    ## 6    16   Edge        NA      2    1654 162.610    1455    161.924        NA
    ##   G3_fr1 G3_pix1  G3_per1 G3_grn1 G3_grnper1 G3_abscnt G4ID G4Pos G4Edge
    ## 1      3    2653  228.108    2339    223.522       365   38    14 Middle
    ## 2     NA      NA       NA      NA         NA       865   72    24   Edge
    ## 3     13   24079 1615.380   19891   1375.800      1524  158    14 Middle
    ## 4      4    7473  394.090    6445    447.588      3906  185    17   Edge
    ## 5     NA      NA       NA      NA         NA       471    7     7 Middle
    ## 6      6   16016  126.930   13682    935.787       267  126     6 Middle
    ##   G4_notes0 G4_fr0 G4_pix0 G4_per0 G4_grn0 G4_grnper0 G4_notes1 G4_fr1 G4_pix1
    ## 1        NA      2    1986 537.254    1662    575.878      <NA>      4    4886
    ## 2        NA     NA      NA      NA      NA         NA      <NA>     NA      NA
    ## 3        NA      2    2912 669.458    2542    653.079      <NA>      8   17689
    ## 4        NA      2    1059 650.344     872    406.636      <NA>      3    3352
    ## 5        NA     NA      NA      NA      NA         NA      <NA>     NA      NA
    ## 6        NA      2    1210 650.617    1005    726.473      <NA>      4    6304
    ##   G4_per1 G4_grn1 G4_grnper1 G4_abscnt G5ID G5Pos G5Edge G5_notes0 G5_fr0
    ## 1 678.715    3235    697.561       344   49     1   Edge        NA      1
    ## 2      NA      NA         NA       206  114    18 Middle        NA     NA
    ## 3 870.638   13453   1047.119       647  151     7 Middle        NA      2
    ## 4 650.344    2227    651.164      1162  180    12   Edge        NA      2
    ## 5      NA      NA         NA       332  187    19 Middle        NA     NA
    ## 6 631.777    4508    772.832       154  209    17   Edge        NA      1
    ##   G5_pix0 G5_per0 G5_grn0 G5_grnper0     G5_notes1 G5_fr1 G5_pix1 G5_per1
    ## 1    1705 644.220    1398    705.609 Stuck on side      2     382 467.215
    ## 2      NA      NA      NA         NA          <NA>     NA      NA      NA
    ## 3    1182 546.637     906    443.015          <NA>      4    2482 522.515
    ## 4    1914 513.311    1585    527.956          <NA>      5    5963 743.163
    ## 5      NA      NA      NA         NA          <NA>     NA      NA      NA
    ## 6    1661 593.798    1484    726.473          <NA>      3    3401 487.175
    ##   G5_grn1 G5_grnper1 G5_abscnt G6ID G6Pos G6Edge G6_notes0 G6_fr0 G6_pix0
    ## 1     203    648.237     308.0  213    21   Edge        NA      2    1979
    ## 2      NA         NA     121.0   67    19 Middle        NA     NA      NA
    ## 3    1060    578.662     287.0  219     3   Edge        NA      2    1530
    ## 4    4746    645.081     245.0  171     3   Edge        NA      2    1086
    ## 5      NA         NA     187.0   65    17   Edge        NA     NA      NA
    ## 6    2414    489.250      64.9   71    23   Edge        NA      2    2195
    ##   G6_per0 G6_grn0 G6_grnper0 G6_notes1 G6_fr1 G6_pix1 G6_per1 G6_grn1
    ## 1 484.561    1731    604.725        NA      6    7666 588.509    6441
    ## 2      NA      NA         NA        NA     NA      NA      NA      NA
    ## 3 523.733    1298    511.208        NA      7    9186 683.975    7760
    ## 4 506.948     890    489.261        NA      3    5495 552.737    4585
    ## 5      NA      NA         NA        NA     NA      NA      NA      NA
    ## 6 552.200    1885    553.418        NA      8   22472 927.478   17743
    ##   G6_grnper1 G6_abscnt G7ID G7Pos G7Edge G7_notes0 G7_fr0 G7_pix0 G7_per0
    ## 1    565.651       245   49     1   Edge        NA      2    1415 492.352
    ## 2         NA       277   64    16   Edge        NA     NA      NA      NA
    ## 3    586.224       686   93    21   Edge        NA      3    2532 561.635
    ## 4    586.438       714   21    21   Edge        NA      2    2059 588.670
    ## 5         NA       281  135    15 Middle        NA     NA      NA      NA
    ## 6    929.006       770   32     8   Edge        NA      2    1330 471.642
    ##   G7_grn0 G7_grnper0 G7_notes1 G7_fr1 G7_pix1 G7_per1 G7_grn1 G7_grnper1
    ## 1    1197    513.462        NA      5    5775 521.848    5142    648.069
    ## 2      NA         NA        NA     NA      NA      NA      NA         NA
    ## 3    2168    646.058        NA      8   24144 931.701   22239   1003.129
    ## 4    1721    453.346        NA      4    6753 633.640    6077    698.068
    ## 5      NA         NA        NA     NA      NA      NA      NA         NA
    ## 6    1148    636.123        NA      6   17003 934.449   14799    877.207
    ##   G7_abscnt G8ID G8Pos G8Edge G8_notes0 G8_fr0 G8_pix0 G8_per0 G8_grn0
    ## 1      1254  176     8   Edge        NA      2    1524 433.540    1332
    ## 2       188  221     5   Edge        NA     NA      NA      NA      NA
    ## 3       449  207    15 Middle        NA      2    1405 499.913    1174
    ## 4      1093   53     5   Edge        NA      2    2046 574.950    1754
    ## 5       187    5     5   Edge        NA     NA      NA      NA      NA
    ## 6       446   38    14 Middle        NA      2    1746 527.956    1649
    ##   G8_grnper0 G8_notes1 G8_fr1 G8_pix1  G8_per1 G8_grn1 G8_grnper1 G8_abscnt
    ## 1    501.694      <NA>      5    4846  825.823    4293    621.507       206
    ## 2         NA      <NA>     NA      NA       NA      NA         NA       900
    ## 3    641.024      <NA>      5   18520  996.221   15004   1028.778       690
    ## 4    452.586      <NA>      5    6087  581.409    5523    654.432       540
    ## 5         NA      <NA>     NA      NA       NA      NA         NA       479
    ## 6    537.254      <NA>      6   19910 1198.303   17433   1039.478       911
    ##   G9ID G9Pos G9Edge G9_notes0 G9_fr0 G9_pix0 G9_per0 G9_grn0 G9_grnper0
    ## 1  215    23   Edge        NA      2    2215 622.124    1957    533.943
    ## 2  147     3   Edge        NA     NA      NA      NA      NA         NA
    ## 3  169     1   Edge        NA      2    1806 532.641    1548    656.478
    ## 4  217     1   Edge        NA      2    1487 566.179    1292    557.704
    ## 5   61    13   Edge        NA     NA      NA      NA      NA         NA
    ## 6   27     3   Edge        NA      2    1947 327.391    1672    445.429
    ##   G9_notes1 G9_fr1 G9_pix1 G9_per1 G9_grn1 G9_grnper1 G9_abscnt G10ID G10Pos
    ## 1        NA      4    2624 581.824    2059    714.849       443   189     21
    ## 2        NA     NA      NA      NA      NA         NA       282   172      4
    ## 3        NA      3    2523 330.331    1705    421.015       116    42     18
    ## 4        NA      5    4573 602.147    3851    549.419       496    44     20
    ## 5        NA     NA      NA      NA      NA         NA       590   130     10
    ## 6        NA      7   20421 994.629   17081    996.244       541   235     19
    ##   G10Edge G10_notes0 G10_fr0 G10_pix0 G10_per0 G10_grn0 G10_grnper0 G10_notes1
    ## 1    Edge         NA       2     2455  463.501     2075     492.272         NA
    ## 2    Edge         NA      NA       NA       NA       NA          NA         NA
    ## 3  Middle         NA       2     1485  506.256     1231     562.482         NA
    ## 4    Edge         NA       2     1983  452.795     1643     474.111         NA
    ## 5  Middle         NA      NA       NA       NA       NA          NA         NA
    ## 6  Middle         NA       2     1692  415.528     1640     693.845         NA
    ##   G10_fr1 G10_pix1 G10_per1 G10_grn1 G10_grnper1 G10_abscnt
    ## 1       6     6461  513.782     5217     812.495       4623
    ## 2      NA       NA       NA       NA          NA       3296
    ## 3       7    21943  981.965    15850     956.553       1726
    ## 4       5     7242  680.212     6383     832.295       2677
    ## 5      NA       NA       NA       NA          NA        712
    ## 6       8    23082 1035.477    18414    1001.604        809

SECTION 3: Data processing. Let’s clean some of this up. This is where
figure legends can be adjusted, etc.

``` r
#Generate legend labels for treatment and populations.
data <- data.frame(lapply(data, function(x) {
  gsub("RegularK", "Ctl", x)
}))
data <- data.frame(lapply(data, function(x) {
  gsub("SaltK", "Salt", x)
}))
data <- data.frame(lapply(data, function(x) {
  gsub("PLT", "Plt", x)
}))
data <- data.frame(lapply(data, function(x) {
  gsub("BAC", "Bac", x)
})) 
data <- data.frame(lapply(data, function(x) {
  gsub("Bac_Plt", "Holo", x)
})) 
data <- data.frame(lapply(data, function(x) {
  gsub("Wellspring", "W", x)
})) 
data <- data.frame(lapply(data, function(x) {
  gsub("Churchill", "C", x)
}))
data <- data.frame(lapply(data, function(x) {
  gsub("RougePark", "R", x)
})) 

# Break dataset into different generations so that we can realign the data
Gen1<- data[,1:20]
Gen2<- data[,c(1:5,22:36)]
Gen3<- data[,c(1:5,38:52)]
Gen4<- data[,c(1:5,54:68)]
Gen5<- data[,c(1:5,70:84)]
Gen6<- data[,c(1:5,86:100)]
Gen7<- data[,c(1:5,102:116)]
Gen8<- data[,c(1:5,118:132)]
Gen9<- data[,c(1:5,134:148)]
Gen10<- data[,c(1:5,150:164)]

# Reassign variable names.
names(Gen1)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen2)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen3)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen4)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen5)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen6)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen7)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen8)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen9)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')
names(Gen10)<-c('Pop','Med','Trt','Rep','g1ID','Pos','Edge','Notes0','Fr0','Pix0','Per0','Grn0','GrnPer0','Notes1','Fr1','Pix1','Per1','Grn1','GrnPer1','AbsCnt')

# Assemble into a single dataset
CWR_1to10<-rbind(Gen1,Gen2,Gen3,Gen4,Gen5,Gen6,Gen7,Gen8,Gen9,Gen10)
CWR_1to10$Gen<-c(rep(1,360),rep(2,360),rep(3,360),rep(4,360),rep(5,360),rep(6,360),rep(7,360),rep(8,360),rep(9,360),rep(10,360))

#Fix structure of data
str(CWR_1to10)
```

    ## 'data.frame':    3600 obs. of  21 variables:
    ##  $ Pop    : chr  "C" "C" "C" "C" ...
    ##  $ Med    : chr  "Salt" "Ctl" "Ctl" "Salt" ...
    ##  $ Trt    : chr  "Plt" "Bac" "Plt" "Plt" ...
    ##  $ Rep    : chr  "7" "2" "8" "2" ...
    ##  $ g1ID   : chr  "5" "9" "10" "11" ...
    ##  $ Pos    : chr  "5" "9" "10" "11" ...
    ##  $ Edge   : chr  "Edge" "Edge" "Middle" "Middle" ...
    ##  $ Notes0 : chr  NA NA NA NA ...
    ##  $ Fr0    : chr  "2" NA "1" "2" ...
    ##  $ Pix0   : chr  "1399" NA "3217" "2231" ...
    ##  $ Per0   : chr  "157.58" NA "219.622" "186.995" ...
    ##  $ Grn0   : chr  "1124" NA "2800" "1889" ...
    ##  $ GrnPer0: chr  "138.853" NA "210.208" "179.924" ...
    ##  $ Notes1 : chr  NA NA NA NA ...
    ##  $ Fr1    : chr  "2" NA "4" "3" ...
    ##  $ Pix1   : chr  "1810" NA "5190" "2723" ...
    ##  $ Per1   : chr  "186.056" NA "503.814" "284.292" ...
    ##  $ Grn1   : chr  "1383" NA "2812" "1968" ...
    ##  $ GrnPer1: chr  "166.409" NA "461.817" "261.806" ...
    ##  $ AbsCnt : chr  "198" "218" "151" "318" ...
    ##  $ Gen    : num  1 1 1 1 1 1 1 1 1 1 ...

``` r
#Fix factorial variables
for (i in 1:7){
  CWR_1to10[,i]<-as.factor(CWR_1to10[,i])
}

#Fix continous variables
for (i in 9:13){
  CWR_1to10[,i]<-as.numeric(CWR_1to10[,i])
}

for (i in 15:21){
  CWR_1to10[,i]<-as.numeric(CWR_1to10[,i])
}

#Let's add a factorial term for generation
CWR_1to10$FacGen<-as.factor(CWR_1to10$Gen)

# Create an interaction term for treatment and media
CWR_1to10$Evol <- paste(CWR_1to10$Trt, CWR_1to10$Med, sep="")
CWR_1to10$Evol<- as.factor(CWR_1to10$Evol)

# Generate summary statistics of interest

# We will use growth rate (increase in surface area) as a proxy for fitness
CWR_1to10$Grt<-CWR_1to10$Pix1 - CWR_1to10$Pix0

# Calculate aggregation, a measure of how closely packed in fronds are. Lots of phenotypic variation in this trait, though its biological significance isn't clear....
# Will be calculated as surface area (# pixels) divided by perimeter, so that less aggregated wells (e.g. high perimeter) have a lower score
CWR_1to10$Agg<-CWR_1to10$Pix1/CWR_1to10$Per1

# We are also going to calculate greeness of the fronds. This could be a fitness-measure, or a trait plausibly associated to variation in growth rate/health.
CWR_1to10$Hlth<-CWR_1to10$Grn1/CWR_1to10$Pix1

# We will calculate mean frond size, just to see if this phenotype is affected by anything. Again, the biological significance of this is still unclear.
CWR_1to10$FrAve<-CWR_1to10$Pix1/CWR_1to10$Fr1

# Let's also calculate the change in number of fronds as an alternative measure of growth.
CWR_1to10$Grt_frd<-CWR_1to10$Fr1-CWR_1to10$Fr0

#Let's create a dataset containing only wells with plants. 
CWR_plt<-subset(CWR_1to10, CWR_1to10$Trt!= "Bac")
CWR_plt<-droplevels(CWR_plt)
```

SECTION 4: data assessment Check normality of data.

``` r
hist(CWR_1to10$Grt, breaks=20)
```

![](Data_analysis_files/figure-gfm/data%20assessment-1.png)<!-- -->

``` r
# That looks OK, let's try breaking this into populations

ggplot(data=CWR_1to10, aes(x = Grt)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ ., scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1218 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-2.png)<!-- -->

``` r
# All have long tails to the right, and R especially has an overabundance of small values.

#Wwhat does this look like across generations?
ggplot(data=CWR_1to10, aes(x = Grt)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Gen, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1218 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-3.png)<!-- -->

``` r
#How about treatment?
ggplot(data=CWR_1to10, aes(x = Grt)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Trt ~ Med, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1218 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-4.png)<!-- -->

``` r
# That looks the best, though I am still curious about logging the data.

CWR_1to10$Log_Grt<-log(CWR_1to10$Grt)
```

    ## Warning in log(CWR_1to10$Grt): NaNs produced

``` r
hist(CWR_1to10$Log_Grt, breaks=20)
```

![](Data_analysis_files/figure-gfm/data%20assessment-5.png)<!-- -->

``` r
#That looks better

ggplot(data=CWR_1to10, aes(x = Log_Grt)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ ., scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1259 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-6.png)<!-- -->

``` r
ggplot(data=CWR_1to10, aes(x = Log_Grt)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Trt ~ Med, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1259 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-7.png)<!-- -->

``` r
# OK yes, this now looks quite normal

## What about changes in frond number?
ggplot(data=CWR_1to10, aes(x = Grt_frd)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Trt ~ Med, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1217 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-8.png)<!-- -->

``` r
#Looks OK

###########################OK, onto aggregation ###############

hist(CWR_1to10$Agg, breaks=20)
```

![](Data_analysis_files/figure-gfm/data%20assessment-9.png)<!-- -->

``` r
ggplot(data=CWR_1to10, aes(x = Agg)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Trt ~ Med, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1210 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-10.png)<!-- -->

``` r
# That actually looks pretty OK.

###########################Health and Greenness? ###############

hist(CWR_1to10$Hlth, breaks=20)
```

![](Data_analysis_files/figure-gfm/data%20assessment-11.png)<!-- -->

``` r
ggplot(data=CWR_1to10, aes(x = Hlth)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Trt ~ Med, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1210 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-12.png)<!-- -->

``` r
# That actually looks pretty OK.

ggplot(data=CWR_1to10, aes(x = Grn1)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Trt ~ Med, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1210 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-13.png)<!-- -->

``` r
# Could log it?

ggplot(data=CWR_1to10, aes(x = log(Grn1))) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Trt ~ Med, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1212 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-14.png)<!-- -->

``` r
# Looks better

###########################Frond size? ###############

hist(CWR_1to10$FrAve, breaks=20)
```

![](Data_analysis_files/figure-gfm/data%20assessment-15.png)<!-- -->

``` r
ggplot(data=CWR_1to10, aes(x = FrAve)) + geom_histogram(fill = "white", colour = "black") +  facet_grid(Pop ~ Trt ~ Med, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1210 rows containing non-finite values (stat_bin).

![](Data_analysis_files/figure-gfm/data%20assessment-16.png)<!-- -->

``` r
# Yeah that looks great.

#OK, so really growth is the only issue here. I've taken the log of it, and we will also try fitting a model with growth as a polynomial term
```

SECTION 5: modelling OK, time for some models.

``` r
CWR_NA<-subset(CWR_plt, CWR_plt$Grt!= "NA")

# 1. Gen - continuous. Pos - edge, random effect. Grt - final pixel#, with initial pix as predictor. med*gen*trt. Pop - random
Mod_grt<-lmer(Pix1 ~ Pix0 + Med*Trt*Gen + (1|Pop) + (1|Edge), data= CWR_plt)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
Mod_grt
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Pix1 ~ Pix0 + Med * Trt * Gen + (1 | Pop) + (1 | Edge)
    ##    Data: CWR_plt
    ## REML criterion at convergence: 49836.69
    ## Random effects:
    ##  Groups   Name        Std.Dev.
    ##  Pop      (Intercept) 6368.4  
    ##  Edge     (Intercept)  784.5  
    ##  Residual             8617.9  
    ## Number of obs: 2382, groups:  Pop, 3; Edge, 2
    ## Fixed Effects:
    ##        (Intercept)                Pix0             MedSalt              TrtPlt  
    ##           5242.990               4.385          -11476.375             -48.190  
    ##                Gen      MedSalt:TrtPlt         MedSalt:Gen          TrtPlt:Gen  
    ##           1341.335            1086.144            -783.251              44.597  
    ## MedSalt:TrtPlt:Gen  
    ##           -117.504  
    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
Anova(Mod_grt, type=3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: Pix1
    ##                 Chisq Df Pr(>Chisq)    
    ## (Intercept)    1.8677  1     0.1717    
    ## Pix0        1006.9942  1  < 2.2e-16 ***
    ## Med          112.2021  1  < 2.2e-16 ***
    ## Trt            0.0020  1     0.9646    
    ## Gen          117.0649  1  < 2.2e-16 ***
    ## Med:Trt        0.5019  1     0.4787    
    ## Med:Gen       20.2288  1  6.871e-06 ***
    ## Trt:Gen        0.0653  1     0.7983    
    ## Med:Trt:Gen    0.2275  1     0.6334    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Significant effects of Pix0, Med, Gen, and Med:Gen.

# Test random effects one at a time? I'm not sure how to test both at the same time...
Mod_grt_pop<-lmer(Pix1 ~ Pix0 + Med*Trt*Gen + (1|Pop), data= CWR_plt)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
exactRLRT(Mod_grt_pop)
```

    ## 
    ##  simulated finite sample distribution of RLRT.
    ##  
    ##  (p-value based on 10000 simulated values)
    ## 
    ## data:  
    ## RLRT = 684.8, p-value < 2.2e-16

``` r
Mod_grt_edge<-lmer(Pix1 ~ Pix0 + Med*Trt*Gen + (1|Edge), data= CWR_plt)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
exactRLRT(Mod_grt_edge)
```

    ## 
    ##  simulated finite sample distribution of RLRT.
    ##  
    ##  (p-value based on 10000 simulated values)
    ## 
    ## data:  
    ## RLRT = 4.2125, p-value = 0.0065

OK, so now we are going to generate Log Response Ratios for some traits…
Reference for calculating LRR’s: Hedges, L. V., Gurevitch, J., & Curtis,
P. S. (1999). The meta-analysis of response ratios in experimental
ecology. Ecology, 80(4), 1150-1156.

``` r
# Effect of inoculating plants with microbial communities
# Start by calculating statistics for each generation for each treatment.
melt_LRR<-melt(CWR_1to10, id.vars=c("Pop", "Gen","Trt","Med"), measure.vars= "Grt", na.rm = T)
Sum_LRR<- ddply(melt_LRR, c("Pop", "Med","Trt","Gen","variable"), summarise,
                mean = mean(value), sd = sd(value), count=length(value),
                sem = sd(value)/sqrt(length(value)))
Sum_LRR_plt<-subset(Sum_LRR, Sum_LRR$Trt!= "BAC")
Sum_LRR_plt<-droplevels(Sum_LRR_plt)
#LRR for Ctl media plants
pop<-3
med<-2
gen<-10

LRR_ctl<-data.frame(matrix(ncol=5, nrow=pop*med*gen))
names(LRR_ctl)<-c('Pop','Med','Gen','LRR','SE')
LRR_ctl$Pop<-c(rep('C', 20), rep('R', 20), rep('W', 20))
LRR_ctl$Med<-c(rep('Ctl', 10), rep('Salt', 10),rep('Ctl', 10), rep('Salt', 10),rep('Ctl', 10), rep('Salt', 10))
Gens<-c(1:10)
LRR_ctl$Gen<-c(rep(Gens, 6))

for (i in 1:10){
  #C, Ctl
  LRR_ctl[i,4]<-log(Sum_LRR_plt[i,6]/Sum_LRR_plt[i+10,6])
  LRR_ctl[i,5]<-sqrt(Sum_LRR_plt[i,9]/Sum_LRR_plt[i,6] + Sum_LRR_plt[i+10,9]/Sum_LRR_plt[i+10,6])
  #C, Salt
  LRR_ctl[i+10,4]<-log(Sum_LRR_plt[i+20,6]/Sum_LRR_plt[i+30,6])
  LRR_ctl[i+10,5]<-sqrt(Sum_LRR_plt[i+20,9]/Sum_LRR_plt[i+20,6] + Sum_LRR_plt[i+30,9]/Sum_LRR_plt[i+30,6])
  #R, Ctl
  LRR_ctl[i+20,4]<-log(Sum_LRR_plt[i+40,6]/Sum_LRR_plt[i+50,6])
  LRR_ctl[i+20,5]<-sqrt(Sum_LRR_plt[i+40,9]/Sum_LRR_plt[i+40,6] + Sum_LRR_plt[i+50,9]/Sum_LRR_plt[i+50,6])
  #R, Salt
  LRR_ctl[i+30,4]<-log(Sum_LRR_plt[i+60,6]/Sum_LRR_plt[i+70,6])
  LRR_ctl[i+30,5]<-sqrt(Sum_LRR_plt[i+60,9]/Sum_LRR_plt[i+60,6] + Sum_LRR_plt[i+70,9]/Sum_LRR_plt[i+70,6])  
  #W, Ctl
  LRR_ctl[i+40,4]<-log(Sum_LRR_plt[i+80,6]/Sum_LRR_plt[i+90,6])
  LRR_ctl[i+40,5]<-sqrt(Sum_LRR_plt[i+80,9]/Sum_LRR_plt[i+80,6] + Sum_LRR_plt[i+90,9]/Sum_LRR_plt[i+90,6])
  #W, Salt
  LRR_ctl[i+50,4]<-log(Sum_LRR_plt[i+100,6]/Sum_LRR_plt[i+110,6])
  LRR_ctl[i+50,5]<-sqrt(Sum_LRR_plt[i+100,9]/Sum_LRR_plt[i+100,6] + Sum_LRR_plt[i+110,9]/Sum_LRR_plt[i+110,6])
  
}
```
