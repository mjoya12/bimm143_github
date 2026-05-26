# Class 7: Machine Learning 1
Mikaela Joya (PID: A17295169)

- [Background](#background)
- [K-means clustering](#k-means-clustering)
- [Principal Component Analysis
  (PCA)](#principal-component-analysis-pca)
  - [1. Analysis of UK Food Data](#1-analysis-of-uk-food-data)
- [Data Import](#data-import)
- [Tidy data](#tidy-data)
- [Exploratory analysis](#exploratory-analysis)
- [PCA to the rescue](#pca-to-the-rescue)
- [Digging deeper (variable
  loadings)](#digging-deeper-variable-loadings)
- [2. PCA of RNA-seq Data](#2-pca-of-rna-seq-data)

## Background

Today we will explore some core machine learning methods that are very
popular in bioinformatics. These include **clustering** and
**dimentionallity reduction**.

## K-means clustering

The main function in “base” R for K-means clustering is called
`kmeans()`

Before we go too deep let’s make up some “simple” dots that we can
cluster and know if e are getting a good answer or not. To do this we
can use the `rnorm()` function:

``` r
hist( rnorm(10000, mean=3) )
```

![](class07_files/figure-commonmark/unnamed-chunk-1-1.png)

``` r
x <- c( rnorm(30, -3), rnorm(30, +3) )
x
```

     [1] -4.0873053 -3.1883986 -2.4537020 -3.3973271 -4.4196911 -4.3860042
     [7] -2.7009187 -3.3337130 -4.7686296 -3.6340319 -5.0986556 -2.2748690
    [13] -2.4097841 -3.6261854 -4.2773463 -1.6144661 -3.0655464 -2.5446535
    [19] -4.6987041 -3.0679472 -2.5601729 -1.8996844 -3.2766715 -4.3845723
    [25] -2.8357435 -2.6985315 -2.8072232 -1.2688520 -2.1594257 -2.3699656
    [31]  2.0484093  3.7285604  2.1301979  1.2090900  3.9865960  2.9839428
    [37]  1.7511542  2.9506571  1.6758951  1.7460000  1.7438907  2.8343892
    [43]  2.4496460  2.0308882  2.3713686  3.2151678  1.9608127  0.8998183
    [49]  4.4071935  3.1369876  2.0679215  2.7351257  3.8556578  4.2261935
    [55]  2.7121293  2.7571308  2.3838084  3.4444537  2.0320571  2.4574355

``` r
z <- cbind(x, y=rev(x))
plot(z)
```

![](class07_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
#rev(x)
```

Now we can run `kmeans()` on this input `z` and see what the results
look like.

``` r
km <- kmeans(z, centers = 2)
km
```

    K-means clustering with 2 clusters of sizes 30, 30

    Cluster means:
              x         y
    1 -3.176957  2.597753
    2  2.597753 -3.176957

    Clustering vector:
     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

    Within cluster sum of squares by cluster:
    [1] 50.68997 50.68997
     (between_SS / total_SS =  90.8 %)

    Available components:

    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      

``` r
attributes(km)
```

    $names
    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      

    $class
    [1] "kmeans"

> Q. How many points are in each cluster?

``` r
km$size
```

    [1] 30 30

> Q. What ‘component’ of your result object details - cluster size? -
> cluster assignment/membership? - cluster center?

``` r
km$cluster
```

     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
km$centers
```

              x         y
    1 -3.176957  2.597753
    2  2.597753 -3.176957

> Q. Plot `z` colored by the kmeans cluster assignment and add cluster
> centers as blue points

``` r
plot(z, col=c("red","blue") )
```

![](class07_files/figure-commonmark/unnamed-chunk-9-1.png)

``` r
plot(z, col=km$cluster)
points(km$centers, col="blue", pch=15)
```

![](class07_files/figure-commonmark/unnamed-chunk-10-1.png)

> Q. Run a K-means clustering and plot the results asking for 4 clusters
> (K=4)?

``` r
km4 <- kmeans(z, centers = 4)
plot(z, col=km4$cluster)
points(km4$centers, col="black", pch=15)
```

![](class07_files/figure-commonmark/unnamed-chunk-11-1.png)

> **N.B.** You need to tell K-means the number of clusters (i.e. set
> `centers=2`)!!

One approach is to try different values for `centers` and then pick the
best…

``` r
ans <- NULL
for(i in 1:10) {
  km <- kmeans(z, centers=i)
  ans <- c(ans, km$tot.withinss)
}

plot(ans, typ="o", 
     xlab="Number of clusters",
     ylab="Total Sum of Squares Distance")
```

![](class07_files/figure-commonmark/unnamed-chunk-12-1.png)

\## Hierarchial Clustering

The main function in “base” R for Hierarchial Clustering is called
`hclust()`

This function does not take your “raw” data for clustering. You must
first build a “distance matrix” from your data and pass this as input to
`hclust()`.

``` r
d <- dist(z)
hc <- hclust(d)
hc
```


    Call:
    hclust(d = d)

    Cluster method   : complete 
    Distance         : euclidean 
    Number of objects: 60 

There is a bespoke `plot()` method for `hclust()` result objects.

``` r
plot(hc)
abline(h=8, col="red")
```

![](class07_files/figure-commonmark/unnamed-chunk-14-1.png)

Once we have our `hclust` object (our “tree” of “cluster dendrogram”) we
can *“cut”* the tree to reveal the clustering pattern.

``` r
cutree(hc, h=8)
```

     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

> Q. Make a plot of `z` with your hclust results (i.e. colored by
> cluster membership)

``` r
grps <- cutree(hc, k=2)
plot(z, col=grps)
```

![](class07_files/figure-commonmark/unnamed-chunk-16-1.png)

## Principal Component Analysis (PCA)

PCA is a dimensionallity reduction method that is popular for revealing
patterns in complex datasets.

### 1. Analysis of UK Food Data

Let’s look at some data on the eating habits of folks from the UK to see
if there are patterns and trends that have some regions being distinct
from others.

## Data Import

The data is made availble in CSV format so we can use the `read.csv()`
function for the import to R:

``` r
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
```

## Tidy data

Fix anything that went wrong with data import.

> **Q1.** How many rows and columns are in your new data frame named x?
> What R functions could you use to answer this questions?

``` r
dim(x)
```

    [1] 17  5

Initially, there are 17 rows and 5 columns. But later when fixed, there
are 17 rows and 4 columns. To answer this: `dim(x)` was used, but
`nrow(x)` and `ncol(x)` could be used too.

Here is the preview of the first 6 rows:

``` r
head(x)
```

                   X England Wales Scotland N.Ireland
    1         Cheese     105   103      103        66
    2  Carcass_meat      245   227      242       267
    3    Other_meat      685   803      750       586
    4           Fish     147   160      122        93
    5 Fats_and_oils      193   235      184       209
    6         Sugars     156   175      147       139

It appears that the row-names are incorrectly set as the first column of
our x data frame (rather than set as proper row-names). Let’s fix this!

``` r
# Note how the minus indexing works
rownames(x) <- x[,1]
x <- x[,-1]
head(x)
```

                   England Wales Scotland N.Ireland
    Cheese             105   103      103        66
    Carcass_meat       245   227      242       267
    Other_meat         685   803      750       586
    Fish               147   160      122        93
    Fats_and_oils      193   235      184       209
    Sugars             156   175      147       139

This looks much better, now lets check the dimensions again:

``` r
dim(x)
```

    [1] 17  4

> N.B. An alternative approach to setting the correct row-names in this
> case would be to read the data file again and this time set the
> row.names argument of read.csv() to be the first column (i.e. use
> argument setting row.names=1), see below:

``` r
x <- read.csv(url, row.names=1)
head(x)
```

                   England Wales Scotland N.Ireland
    Cheese             105   103      103        66
    Carcass_meat       245   227      242       267
    Other_meat         685   803      750       586
    Fish               147   160      122        93
    Fats_and_oils      193   235      184       209
    Sugars             156   175      147       139

> **Q2.** Which approach to solving the ‘row-names problem’ mentioned
> above do you prefer and why? Is one approach more robust than another
> under certain circumstances?

I prefer using `x <- read.csv(url, row.names=1)` because it fixes the
row names when imported avoiding extra steps. This is more robust
because if `x <- x[,-1]` is run multiple times, it will keep removing
columns and will mess up the data.

## Exploratory analysis

Make some plots to help make sense of obvious trends…

``` r
# Using base R
barplot(as.matrix(x), beside=T, col=rainbow(nrow(x)))
```

![](class07_files/figure-commonmark/unnamed-chunk-23-1.png)

> **Q3.** Changing what optional argument in the above barplot()
> function results in the following plot?

``` r
# Using base R
barplot(as.matrix(x), beside=F, col=rainbow(nrow(x)))
```

![](class07_files/figure-commonmark/unnamed-chunk-24-1.png)

Changing the `beside` argument results in the plot changing from a
grouped barplot to a stacked barplot. When `beside = TRUE`, the food
categories are shown next to each other for each country. While,
`beside = FALSE` makes the food categories stacked combining each
country.

``` r
library(tidyr)

# Convert data to long format for ggplot with `pivot_longer()`
x_long <- x |> 
          tibble::rownames_to_column("Food") |> 
          pivot_longer(cols = -Food, 
                       names_to = "Country", 
                       values_to = "Consumption")

dim(x_long)
```

    [1] 68  3

``` r
# Create grouped bar plot
library(ggplot2)

ggplot(x_long) +
  aes(x = Country, y = Consumption, fill = Food) +
  geom_col(position = "dodge") +
  theme_bw()
```

![](class07_files/figure-commonmark/unnamed-chunk-26-1.png)

> **Q4.** Changing what optional argument in the above ggplot() code
> results in a stacked barplot figure?

Chagning the position argument in `geom_col()` from `"dodge"` to
`"stack"` results in a stacked barplot figure.

``` r
library(ggplot2)

ggplot(x_long) +
  aes(x = Country, y = Consumption, fill = Food) +
  geom_col(position = "stack") +
  theme_bw()
```

![](class07_files/figure-commonmark/unnamed-chunk-27-1.png)

> **Q5.** We can use the pairs() function to generate all pairwise plots
> for our countries. Can you make sense of the following code and
> resulting figure? What does it mean if a given point lies on the
> diagonal for a given plot?

``` r
pairs(x, col=rainbow(nrow(x)), pch=16)
```

![](class07_files/figure-commonmark/unnamed-chunk-28-1.png)

The `pairs()` function creates scatter plots that compare each country
to every other country. Each point is a food type, and if a point is
close to the diagonal it means that the food was consumed in similar
amounts in the countries compared.

Another useful visualization approach that is frequently used in
bioinformatics is a heatmap.

``` r
library(pheatmap)
pheatmap( as.matrix(x) )
```

![](class07_files/figure-commonmark/unnamed-chunk-29-1.png)

> **Q6.** Based on the pairs and heatmap figures, which countries
> cluster together and what does this suggest about their food
> consumption patterns? Can you easily tell what the main differences
> between N. Ireland and the other countries of the UK in terms of this
> data-set?

The countries that cluster together more closely (having similar food
consumption patterns) in the heat map are England, Wales, and Scotland.
Northern Island is more separated from the other countries (has
different food consumption patterns). This is especially true because
when looking at the heatmap, Northern Ireland is different in foods such
as fresh potatoes, fresh fruit, and alcoholic drinks.

> **Key-point**: Even relatively small datasets can prove challenging to
> interpret Given that it is quite difficult to make sense of even this
> relatively small data set. Hopefully, we can clearly see that a
> powerful analytical method is absolutely necessary if we wish to
> observe trends and patterns in larger datasets.

## PCA to the rescue

The main function in “base” R for PCA is called `prcomp()`. This
function wants the “observations” to be rows and the “variables” to the
columns.

So here we need to take the transpose of our `x` input object

``` r
pca <- prcomp ( t(x) )
summary(pca)
```

    Importance of components:
                                PC1      PC2      PC3       PC4
    Standard deviation     324.1502 212.7478 73.87622 2.921e-14
    Proportion of Variance   0.6744   0.2905  0.03503 0.000e+00
    Cumulative Proportion    0.6744   0.9650  1.00000 1.000e+00

The returned `pca` object has components that we can use to make our
main results figures:

``` r
attributes(pca)
```

    $names
    [1] "sdev"     "rotation" "center"   "scale"    "x"       

    $class
    [1] "prcomp"

The main result figure from this analysis is called a **“PC score
plot”** (a.k.a an “ordination plot,”PC plot”, or simply “PC1 vs PC2
plot”.

This plot shows how samples (in this case countries) relate to each
other along our new PC axis.

This is our new “reduced-dimensional space”. In this case 2 dimensions,
PC1 and PC2, that capture most of the variance in the original 17
dimensional data-set.

``` r
library(ggplot2)

ggplot(pca$x) +
  aes(PC1, PC2) +
  geom_point()
```

![](class07_files/figure-commonmark/unnamed-chunk-32-1.png)

``` r
mycols <- c("orange", "red", "blue", "darkgreen")

ggplot(pca$x) +
  aes(PC1, PC2, label=row.names(pca$x)) +
  geom_point(col=mycols)
```

![](class07_files/figure-commonmark/unnamed-chunk-33-1.png)

``` r
  geom_text(size=3, vjust=2, col=mycols)
```

    geom_text: na.rm = FALSE, parse = FALSE, check_overlap = FALSE, size.unit = mm
    stat_identity: na.rm = FALSE
    position_nudge 

> **Q7**. Complete the code below to generate a plot of PC1 vs PC2. The
> second line adds text labels over the data points.

``` r
# Create a data frame for plotting
df <- as.data.frame(pca$x)
df$Country <- rownames(df)

# Plot PC1 vs PC2 with ggplot
ggplot(pca$x) +
  aes(x = PC1, y = PC2, label = rownames(pca$x)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  xlim(-270, 500) +
  xlab("PC1") +
  ylab("PC2") +
  theme_bw()
```

![](class07_files/figure-commonmark/unnamed-chunk-34-1.png)

> **Q8.** Customize your plot so that the colors of the country names
> match the colors in our UK and Ireland map and table at start of this
> document.

``` r
country_colors <- c("England" = "red","Wales" = "darkgreen",
  "Scotland" = "blue", "N.Ireland" = "orange")

ggplot(df) +
  aes(x = PC1, y = PC2, label = Country, color = Country) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  scale_color_manual(values = country_colors) +
  xlim(-270, 500) +
  xlab("PC1") +
  ylab("PC2") +
  theme_bw()
```

![](class07_files/figure-commonmark/unnamed-chunk-35-1.png)

## Digging deeper (variable loadings)

We can also consider the influence of each of the original variables
upon the principal components (typically known as loading scores). This
information can be obtained from the `prcomp()` returned `$rotation`
component. It can also be summarized with a call to `biplot()`, see
below:

``` r
ggplot(pca$rotation) +
  aes(PC1, 
      reorder(row.names(pca$rotation), PC1)) +
  geom_col(fill = "steelblue") +
  xlab("PC1 Loading Score") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text (size = 9))
```

![](class07_files/figure-commonmark/unnamed-chunk-36-1.png)

Here we see observations (foods) with the largest positive loading
scores that effectively “push” N. Ireland to right positive side of the
plot (including Fresh_potatoes and Soft_drinks).

We can also see the observations/foods with high negative scores that
push the other countries to the left side of the plot (including
Fresh_fruit and Alcoholic_drinks).

> **Q9.** Generate a similar ‘loadings plot’ for PC2. What two food
> groups feature prominantely and what does PC2 maninly tell us about?

``` r
ggplot(pca$rotation) +
  aes(x = PC2, 
      y = reorder(rownames(pca$rotation), PC2)) +
  geom_col(fill = "steelblue") +
  xlab("PC2 Loading Score") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 9))
```

![](class07_files/figure-commonmark/unnamed-chunk-37-1.png)

For PC2, the food groups that are prominently featured are the soft
drinks and fresh potatoes/fresh fruit (it depends which side of PCS
loaded). Furthermore, PC2 mainly tells us about the foods that separate
the countries along the second largest variation (after PC1), explaining
the smaller differences between countries that PC1 doesn’t.

> **Key-point:** Key-Point: PCA has the awesome ability to effectively
> reduce the dimensionality of our data set down from 17 to 2, allowing
> us to assert (using our figures above) that countries England, Wales
> and Scotland are ‘similar’ with Northern Ireland being different in
> some way. Furthermore, digging deeper into the loadings we were able
> to associate certain food types with each cluster of countries. This
> is supper useful!

## 2. PCA of RNA-seq Data

RNA-seq results often contain a PCA (or related MDS plot). Usually we
use these graphs to verify that the control samples cluster together.

``` r
url2 <- "https://tinyurl.com/expression-CSV"
rna.data <- read.csv(url2, row.names=1)
head(rna.data)
```

           wt1 wt2  wt3  wt4 wt5 ko1 ko2 ko3 ko4 ko5
    gene1  439 458  408  429 420  90  88  86  90  93
    gene2  219 200  204  210 187 427 423 434 433 426
    gene3 1006 989 1030 1017 973 252 237 238 226 210
    gene4  783 792  829  856 760 849 856 835 885 894
    gene5  181 249  204  244 225 277 305 272 270 279
    gene6  460 502  491  491 493 612 594 577 618 638

``` r
dim(rna.data)
```

    [1] 100  10

``` r
## Again we have to take the transpose of our data 
pca <- prcomp(t(rna.data), scale=TRUE)

# Create data frame for plotting
df <- as.data.frame(pca$x)
df$Sample <- rownames(df)

## Plot with ggplot
ggplot(df) +
  aes(x = PC1, y = PC2, label = Sample) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  xlab("PC1") +
  ylab("PC2") +
  theme_bw()
```

![](class07_files/figure-commonmark/unnamed-chunk-40-1.png)

``` r
summary(pca)
```

    Importance of components:
                              PC1    PC2     PC3     PC4     PC5     PC6     PC7
    Standard deviation     9.6237 1.5198 1.05787 1.05203 0.88062 0.82545 0.80111
    Proportion of Variance 0.9262 0.0231 0.01119 0.01107 0.00775 0.00681 0.00642
    Cumulative Proportion  0.9262 0.9493 0.96045 0.97152 0.97928 0.98609 0.99251
                               PC8     PC9    PC10
    Standard deviation     0.62065 0.60342 3.3e-15
    Proportion of Variance 0.00385 0.00364 0.0e+00
    Cumulative Proportion  0.99636 1.00000 1.0e+00

``` r
# Calculate variance explained
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

# Create scree plot data
scree_df <- data.frame(
  PC = factor(paste0("PC", 1:10), levels = paste0("PC", 1:10)),
  Variance = pca.var[1:10]
)

ggplot(scree_df) +
  aes(x = PC, y = Variance) +
  geom_col(fill = "steelblue") +
  ggtitle("Quick scree plot") +
  xlab("Principal Component") +
  ylab("Variance") +
  theme_bw()
```

![](class07_files/figure-commonmark/unnamed-chunk-42-1.png)

Let’s make another version showing percent variation:

``` r
## Percent variance is often more informative to look at 
pca.var.per
```

     [1] 92.6  2.3  1.1  1.1  0.8  0.7  0.6  0.4  0.4  0.0

``` r
# Create percent variance scree plot
scree_pct_df <- data.frame(
  PC = factor(paste0("PC", 1:10), levels = paste0("PC", 1:10)),
  PercentVariation = pca.var.per[1:10]
)

ggplot(scree_pct_df) +
  aes(x = PC, y = PercentVariation) +
  geom_col(fill = "steelblue") +
  ggtitle("Scree Plot") +
  xlab("Principal Component") +
  ylab("Percent Variation") +
  theme_bw()
```

![](class07_files/figure-commonmark/unnamed-chunk-44-1.png)

> **Q10.** How many genes and samples are in this data set? How many PCs
> do you think it will take to have a useful overview of this data set
> (see above)?

The RNA-seq dataset has 100 genes and 10 samples. Since PC1 explains
92.6% of the variance, 1 PC is already very useful. Therefore using PC1
and PC2 together gives a better overview because they explain about
94.9% of the variance in total.

Now lets make our main PCA plot a bit more attractive and useful…

``` r
## A vector of colors for wt and ko samples
colvec <- colnames(rna.data)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"

# Add condition to data frame
df$condition <- substr(df$Sample, 1, 2)
df$color <- colvec

ggplot(df) +
  aes(x = PC1, y = PC2, color = color, label = Sample) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5, show.legend = FALSE) +
  scale_color_identity() +
  xlab(paste0("PC1 (", pca.var.per[1], "%)")) +
  ylab(paste0("PC2 (", pca.var.per[2], "%)")) +
  theme_bw()
```

![](class07_files/figure-commonmark/unnamed-chunk-45-1.png)
