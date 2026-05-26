# Class 12 Lab Session
Mikaela Joya (PID: A17295169)

- [Section 1. Proportion of G/G in a
  population](#section-1-proportion-of-gg-in-a-population)
- [Section 4: Population Scale
  Analysis](#section-4-population-scale-analysis)

# Section 1. Proportion of G/G in a population

Downloaded a CSV file from Ensemble \<
https://www.ensembl.org/Homo_sapiens/Variation/Sample?db=core;r=17:39894595-39895595;v=rs8067378;vdb=variation;vf=959672880#373531_tablePanel
\>

Here we read this CSV file

``` r
mxl <- read.csv("373531-SampleGenotypes-Homo_sapiens_Variation_Sample_rs8067378.csv")
head(mxl)
```

      Sample..Male.Female.Unknown. Genotype..forward.strand. Population.s. Father
    1                  NA19648 (F)                       A|A ALL, AMR, MXL      -
    2                  NA19649 (M)                       G|G ALL, AMR, MXL      -
    3                  NA19651 (F)                       A|A ALL, AMR, MXL      -
    4                  NA19652 (M)                       G|G ALL, AMR, MXL      -
    5                  NA19654 (F)                       G|G ALL, AMR, MXL      -
    6                  NA19655 (M)                       A|G ALL, AMR, MXL      -
      Mother
    1      -
    2      -
    3      -
    4      -
    5      -
    6      -

``` r
table(mxl$Genotype..forward.strand.)
```


    A|A A|G G|A G|G 
     22  21  12   9 

``` r
table(mxl$Genotype..forward.strand.) / nrow(mxl) * 100
```


        A|A     A|G     G|A     G|G 
    34.3750 32.8125 18.7500 14.0625 

Now let’s look at a different population. I picked the GBR.

``` r
gbr <- read.csv("373522-SampleGenotypes-Homo_sapiens_Variation_Sample_rs8067378.csv")
head(gbr)
```

      Sample..Male.Female.Unknown. Genotype..forward.strand. Population.s. Father
    1                  HG00096 (M)                       A|A ALL, EUR, GBR      -
    2                  HG00097 (F)                       G|A ALL, EUR, GBR      -
    3                  HG00099 (F)                       G|G ALL, EUR, GBR      -
    4                  HG00100 (F)                       A|A ALL, EUR, GBR      -
    5                  HG00101 (M)                       A|A ALL, EUR, GBR      -
    6                  HG00102 (F)                       A|A ALL, EUR, GBR      -
      Mother
    1      -
    2      -
    3      -
    4      -
    5      -
    6      -

Find proportion of G\|G

``` r
round(table(gbr$Genotype..forward.strand.) /nrow(gbr) * 100, 2 )
```


      A|A   A|G   G|A   G|G 
    25.27 18.68 26.37 29.67 

This variant that is associated with childhood asthma is more frequent
in the GBR population than the MKL population.

Let’s now dig into this further.

# Section 4: Population Scale Analysis

> **Q13.** Read this file into R and determine the sample size for each
> genotype and their corresponding median expression levels for each of
> these genotypes.

``` r
file <- "rs8067378_ENSG00000172057.6.txt"
data <- read.table(file)

colnames(data) <- c("sample", "genotype", "expression")

head(data)
```

       sample genotype expression
    1 HG00367      A/G   28.96038
    2 NA20768      A/G   20.24449
    3 HG00361      A/A   31.32628
    4 HG00135      A/A   34.11169
    5 NA18870      G/G   18.25141
    6 NA11993      A/A   32.89721

``` r
table(data$genotype)
```


    A/A A/G G/G 
    108 233 121 

The sample size for the A/A genotype is 108, the A/G genotype is 233,
and G/G genotype is 121.

``` r
tapply(data$expression, data$genotype, median)
```

         A/A      A/G      G/G 
    31.24847 25.06486 20.07363 

The corresponding median expressions are 31.24847 (A/A), 25.06486 (A/G),
and 20.07363 (G/G).

> **Q14.** Generate a boxplot with a box per genotype, what could you
> infer from the relative expression value between A/A and G/G displayed
> in this plot? Does the SNP effect the expression of ORMDL3?

``` r
library(ggplot2)

ggplot(data,
       aes(x = genotype, y = expression, fill = genotype)) + 
       geom_boxplot(notch = TRUE) + 
       geom_jitter(width = 0.15, alpha = 0.5) +
  labs(
    title = "ORMDL3 Expression by Genotype",
    x = "Genotype",
    y = "Expression") +
  theme_gray()
```

![](class12_files/figure-commonmark/unnamed-chunk-9-1.png)

This boxplot shows that the A/A genotype individuals have higher
expression levels of ORMDL3 compared to those with G/G. This means that
the rs8067378 SNP affects the expression of the ORMDL3 gene because the
G allele is associated with a decrease in gene expression.
